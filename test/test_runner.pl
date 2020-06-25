#!/usr/bin/perl
# Copyright (c) 2014, Jeff Cohen
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

use threads;
use threads::shared;

use Time::HiRes qw(time);
my $startTime = time;

my @options = grep(/^-/, @ARGV);
my @files = grep(/^[^-]/, @ARGV);

my $useRelease = grep(/^--release/, @options);
my $noEXE = grep(/^--no-exe/, @options);
@_ = grep(/^--jobs/, @options);
my $jobs = pop @_;

if ($jobs) {
    if ($jobs =~ /--jobs=(\d+)$/) {
        $jobs = $1;
    } else {
        die "Bad --jobs option: $jobs\n";
    }
} else {
    $jobs = $ENV{"JOLT_TR_JOBS"} || 1;
}

@files = ( "." ) unless @files > 0;

my %allTests;

sub addTest {
    ($_) = @_;
    return if not /\.test$/;
    s/^\.\///;
    s/\\/\//g;
    $allTests{$_} = 1;
}

sub recurse {
    my ($path, $file) = @_;
    $file = $path . $file;

    opendir(DIR, $file) || die "$!: $file\n";
    my @files = grep(/^[^.]/, readdir DIR);
    closedir(DIR);

    foreach $f (@files) {
        if (-d $f) {
            &recurse($file . "/", $f);
        } else {
            &addTest($file . "/" . $f);
        }
    }
}

foreach $file (@files) {
    if (-d $file) {
        &recurse("", $file);
    } else {
        &addTest($file);
    }
}

my $windows = $^O eq "MSWin32";
my $macos = $^O eq "darwin";
my $use_clang = $^O eq "freebsd";
my $mode = $useRelease ? "Release" : "Debug";
my $devnull = $windows ? "" : "2>/dev/null";

# Locate jolt compiler.
my $compiler;
if ($windows) {
    $compiler = "../build/windows/jolt";
} else {
    if ($macos && -e "../build/macos/xcode.products.dir") {
        open(FILE, "../build/macos/xcode.products.dir") || die "$!: xcode.products.dir\n";
        my $xcode = <FILE>;
        chomp $xcode;
        close(FILE);

        if (-e "$xcode/jolt") {
            $compiler = "$xcode/jolt";
        }
    }

    if (! $compiler) {
        $compiler = "../frontend";
        undef $macos;
    }
}

my $segments = 0;
my $prefix = "";
if (! $macos) {
    while ($segments < 5 && !-e $compiler) {
        $segments++;
        $compiler = "../$compiler";
        $prefix = "../$prefix";
    }

    if ($windows) {
        if (-e "$compiler/x64/$mode/jolt.exe") {
            $compiler = "$compiler/x64/$mode/jolt";
        } else {
            $compiler = "$compiler/$mode/jolt";
        }
    } else {
        $compiler = "$compiler/$mode/jolt";
    }

    if ($segments >= 5 || !-e ("$compiler" . ($windows ? ".exe" : ""))) {
        die "ERROR: Cannot find Jolt compiler!\n";
    }
}

sub verifyTestResults {
    my ($name, $tests, $results, $epoch) = @_;
    my $first = "\n";

    my $phase = "both";
    my $count = 2;  # assume no internal compiler error or
    my $pass = 2;   # extra test results

    my %results;
    foreach $_ (@$results) {
        chomp;
        if (/^FATAL: Verify failure/) {
            print OUT "Internal compiler error detected: $_\n";
            $pass--;
        } elsif (/^.*?jolt(\(\d+:\d+\)): (\w+): (.*)/) {
            $results{"$2$1"} = $3;
        } elsif (/^(\S+):\s*(..*?)\s*$/) {
            $results{$1} = $2;
        }
    }

    foreach $_ (@$tests) {
        if (/^\s*epoch\s+(compile|run|both)\s*$/) {
            $phase = $1;
            next;
        }
        
        next if ($phase ne $epoch and $phase ne "both");

        if (/^\s*assert_eq\s+(\S+)\s+(.*)\s*$/) {
            $count++;
            if ($results{$1} eq $2) {
                delete $results{$1};
                $pass++;
            } else {
                print OUT "$first$name: $1: expected '$2', found '$results{$1}'\n";
                $first = "";
            }
        } elsif (/^\s*assert_nil\s+(\S+)\s*$/) {
            $count++;
            if (!defined $results{$1}) {
                $pass++;
            } else {
                print OUT "$first$name: $1: expected nil, found '$results{$1}'\n";
                delete $results{$1};
                $first = "";
            }
        } else {
            $count++;
            print OUT "$first$name: syntax error: $_\n";
            $first = "";
        }
    }

    if (scalar(keys %results) > 0) {
        print OUT "Extra results seen: ", join(" ", sort keys %results), "\n";
        $pass--;
    }

    return $count, $pass;
}

my $ntests :shared = 0;
my $count :shared = 0;
my $pass :shared = 0;

sub runTest {
    my ($test) = @_;

    my $src;
    my @tests;
    my @import;
    my $compileOnly = 0;

    my $path = $test;
    $path =~ s|[^/]*$||;

    open(FILE, $test) || die "$!: $test\n";
    while (<FILE>) {
        chomp;
        next if /^\s*#/;
        next if /^\s*$/;

        if (/^source\s+(.*)\s*/) {
            $src = join(' ', map { $path . $_ } split(/ /, $1));
        } elsif (/^import\s+(.*)\s*/) {
            @import = map { $path . $_ } split(/ /, $1);
        } elsif (/^compileonly\s*$/) {
            $compileOnly = 1;
        } else {
            push @tests, $_;
        }
    }
    close(FILE);

    $test =~ /\.test$/;
    my $out = $` . ".out";
    my $exe = $` . ($windows ? ".exe" : "");

    unlink $exe;
    unlink $out;

    open(OUT, ">$out") || die "$!: $out\n";

    foreach $module (@import) {
        my $cmd = "$compiler $module -I $path";
        print OUT "\n**** $cmd\n\n";

        open(CMD, "$cmd $devnull |") || die "$!: $cmd";
        my @results = <CMD>;
        close(CMD);
        print OUT @results;
    }

    my $cmd = "$compiler $src -I $path -o $exe";
    print OUT "\n**** $cmd\n\n";

    open(CMD, "$cmd $devnull |") || die "$!: $cmd";
    my @results = <CMD>;
    close(CMD);
    print OUT @results;

    my $obj = $windows ? ".obj" : ".o";
    foreach $module (@import) {
        my $file = $module;

        $file =~ s/\.jolt$/.jmod/;
        unlink $file;

        $file =~ s/\.jmod$/$obj/;
        unlink $file;
    }

    my ($c, $p) = &verifyTestResults($test, \@tests, \@results, "compile");
    {
        lock($count);
        $count += $c;
        $pass += $p;
    }

    my $passed = ($c == $p);

    if (!$compileOnly && !$noEXE) {
        if (!-e $exe) {
           close(OUT);
           $count++;
           return $out;
        }

        print OUT "\n\n**** Executing $exe\n\n";
        if (open(CMD, "$exe |")) {
          @results = <CMD>;
          close(CMD);
          print OUT @results;
        } else {
          print OUT "$!: $exe\n";
          @results = ( );
        }

        ($c, $p) = &verifyTestResults($test, \@tests, \@results, "run");
        {
            lock($count);
            $count += $c;
            $pass += $p;
        }
        $passed = 0 unless ($c == $p);
    }

    unlink $exe;

    close(OUT);
    return $passed ? "" : $out;
}

my @testList : shared = sort keys %allTests;
my @failedtests : shared = ();
my $running : shared;

sub reportFailure {
    my ($test) = @_;
    lock(@failedtests);
    push(@failedtests, $test);
}

if ($jobs == 1) {
    foreach $test (@testList) {
        $ntests++;

        my $out = &runTest($test);

        if ($out) {
            print "*";
            my $x = $test;
            $x =~ s/\.test$//;
            push(@failedtests, $x);
        } else {
            print ".";
        }
    }
} else {
    $running = $jobs;
    for (my $i = 0; $i < $jobs; $i++) {
        threads->create(sub {
            {
                for (;;) {
                    my $test;
                    {
                        lock(@testList);
                        unless (@testList) {
                            lock($running);
                            $running--;
                            cond_signal($running);
                            return;
                        }
                        $test = shift @testList;
                        $ntests++;
                    }

                    my $out = &runTest($test);

                    lock(@testList);
                    if ($out) {
                        print "*";
                        my $x = $test;
                        $x =~ s/\.test$//;
                        push(@failedtests, $x);
                    } else {
                        print ".";
                    }
                }
            }
        })->detach();
    }

    lock($running);
    cond_wait($running) while $running;
}

if (@failedtests > 0) {
    print "\n\nThe following tests failed:\n";
    print join("\n", @failedtests);
}

my $elapsed = sprintf("%10.3f", time - $startTime);

my $fail = $count - $pass;
print "\n\n$ntests tests, $count assertions, $pass passed, $fail failed\n";
print "\n$elapsed seconds runtime\n\n";
