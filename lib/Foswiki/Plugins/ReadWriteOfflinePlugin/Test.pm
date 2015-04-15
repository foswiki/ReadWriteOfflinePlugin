# WRC466130775A3661534B79534655
#
# Copyright (C) 2006-2008 Crawford Currie http://wikiring.com
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# This notice must be retained in all copies and derivatives of
# this code.
#
# Because of the nature of this plugin we can't do a conventional
# unit test strategy. So instead, the plugin has internal test capability
# built into this module. The test can be run entirely on this server, no
# second server is required.
#
# The test is run by having the =test= REST handler invoked from the
# browser. The test will create its own fixtures, and will not pollute
# any data already existing on the server.
#
# Run it thus:
# http://mywiki.com/bin/rest/ReadWriteOfflinePlugin/test?username=User;password=Pass
#

package Blah;

sub new {
    my ($class, $syncher) = @_;
    return bless({ syncher => $syncher}, $class);
}

sub report {
    my ($this, $message, $level) = @_;
    return unless $message;
    $level ||= 0;
    my $colour;
    if ($level == 2) {
        $colour='red';
    } elsif ($level == 1) {
        $colour = 'orange';
    } elsif ($level < 0) {
        $colour = 'black';
    } else {
        $colour = '#00AA00';
    }
    my $rt = Foswiki::Time::formatTime($this->{syncher}->{serverTime}, 'iso');
    my $lt = Foswiki::Time::formatTime(time(), 'iso');
    print "<font color='$colour'>$lt ($rt): $message</font><br />\n";
};

package BlahShh;

sub new {
    my ($class, $ref) = @_;
    return bless({ref => $ref}, $class);
}

sub report {
    my ($this, $message, $level) = @_;
    if ($level) {
        ${$this->{ref}} .= "$level: $message\n";
    }
};

package Foswiki::Plugins::ReadWriteOfflinePlugin::Test;

use strict;
use Error;
use Assert;
use CGI::Carp qw(fatalsToBrowser);
use Foswiki;

use Foswiki::Plugins::ReadWriteOfflinePlugin::UserAgent;
use Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher;
use File::Path;

my $session;
my $prefix = 'ReadWriteOfflineTest_';
my $local_web = $prefix.'Local';
my $remote_web = $prefix.'Remote';
my $remote_web2 = $prefix.'Remote2';
my $syncher;
my $loopback = 0;

use vars qw($output);
my $grabber = new BlahShh(\$output);

sub run {
    my ($role, $rid, $action) = @_;
    die "No RID" unless $rid;

    $session = $Foswiki::Plugins::SESSION;

    if ($role eq 'passive') {
        print CGI::header('text/plain');
        if ($action eq 'init') {
            # Webs may exist if a prior run failed (active and passive)
            my $dir = Foswiki::Func::getWorkArea('ReadWriteOfflinePlugin');
            chuck(glob "$Foswiki::cfg{PubDir}/$prefix*");
            chuck(glob "$Foswiki::cfg{DataDir}/$prefix*");
        } elsif ($action eq 'set_up') {
            set_up($rid, 1);
        } elsif ($action eq 'tear_down') {
            tear_down($rid, 1);
        } elsif ($action eq 'move') {
            move(1);
        } else {
            die "Bad action ".($action||'undef');
        }
        print "OK";
        return;
    }

    $| = 1; # autoflush on

    # Webs may exist if a prior run failed (active and passive)
    my $dir = Foswiki::Func::getWorkArea('ReadWriteOfflinePlugin');
    chuck(glob "$Foswiki::cfg{PubDir}/$prefix*");
    chuck(glob "$Foswiki::cfg{DataDir}/$prefix*");

    $SIG{'__WARN__'} = sub { die @_ };
    $Error::Debug = 1; # verbose stack traces, please

    print CGI::header('text/html');

    die "\$Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID} not defined"
      unless $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID};

    my $rwop = $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers}->{$rid};
    $rwop ||= {};
    $rwop->{DefaultUrlHost} ||= $Foswiki::cfg{DefaultUrlHost};
    $rwop->{ScriptUrlPath} ||= $Foswiki::cfg{ScriptUrlPath};
    $rwop->{ScriptSuffix} ||= $Foswiki::cfg{ScriptSuffix};
    $rwop->{Username} ||= Foswiki::Func::getCgiQuery()->param('username');
    $rwop->{Password} ||= Foswiki::Func::getCgiQuery()->param('password');
    $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{Servers}->{$rid} = $rwop;
    $loopback = ($rid eq 'loopback');

    $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $session, $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID},
        'tx');
    my $reporter = new Blah($syncher);
    die "Failed" unless $syncher->setRemoteServer($rid, $reporter);
    $syncher->testOnRemote("init");

    my @tests = sort grep { /^test_/ }
        keys %Foswiki::Plugins::ReadWriteOfflinePlugin::Test::;

    print CGI::p(<<HERE);
If you do not see "All tests completed" at the
end of this run, then a test failed. You may have to refer to logfiles
on the peer node to determine what went wrong.
HERE
    # The tests are executed in order, but no later test should depend
    # on the results of an earlier test.
    foreach my $test (@tests) {

        $reporter->report("Running $test", 1);
        $test = "Foswiki::Plugins::ReadWriteOfflinePlugin::Test::$test";
        set_up($rid, 0);
        no strict 'refs';
        &$test($syncher);
        use strict 'refs';
        tear_down($rid, 0);
    }

    $reporter->report("All tests completed");
}

sub chuck {
    foreach my $file (@_) {
        $file =~ /(.*)/; # untaint
        File::Path::rmtree($1);
    }
}

sub set_up {
    my ($rid, $passive) = @_;

    my $dir = Foswiki::Func::getWorkArea('ReadWriteOfflinePlugin');
    chuck(glob "$dir/*.$rid.synchDB");

    if ($passive) {
        Foswiki::Func::createWeb($remote_web);
        return;
    }

    # create the standard fixture:
    # +- $local_web/
    # |       |
    # |       +-- SubWeb/
    # |       |      |
    # |       |      +-- WebPreferences
    # |       |
    # |       +-- TestTopic1@2
    # |       |      |
    # |       |      +-- Att1.txt
    # |       |
    # |       +-- WebPreferences
    # |       |
    # |       +-- ViewDeniedAll
    # |       |
    # |       +-- ChangeDeniedAll
    # |       |
    # |       +-- ViewDeniedOthers
    # |       |
    # |       +-- ChangeDeniedOthers
    # |
    # +-- $remote_web/
    #
    Foswiki::Func::createWeb($local_web);
    Foswiki::Func::createWeb("$local_web/SubWeb");

    my $user = Foswiki::Func::getWikiName();

    Foswiki::Func::saveTopic(
        $local_web, "WebPreferences", undef,
        "No preferences");
    Foswiki::Func::saveTopic(
        $local_web, "ViewDeniedAll", undef,
        "   * Set ALLOWTOPICVIEW = nobody");
    Foswiki::Func::saveTopic(
        $local_web, "ChangeDeniedAll", undef,
        "   * Set ALLOWTOPICCHANGE = nobody");
    Foswiki::Func::saveTopic(
        $local_web, "ViewDeniedOthers", undef,
        "   * Set ALLOWTOPICVIEW = $user");
    Foswiki::Func::saveTopic(
        $local_web, "ChangeDeniedOthers", undef,
        "   * Set ALLOWTOPICCHANGE = $user");
    Foswiki::Func::saveTopic(
        "$local_web/SubWeb", "WebPreferences", undef,
        "No subweb preferences");
    Foswiki::Func::saveTopic(
        $local_web, "TestTopic1", undef,
        "Rev1");
    Foswiki::Func::saveAttachment(
        $local_web, "TestTopic1",
        "Att1.txt",
        { file=> "$Foswiki::cfg{DataDir}/Sandbox/WebPreferences.txt" });
    Foswiki::Func::saveTopic(
        $local_web, "TestTopic1", undef,
        "Rev2", {forcenewrevision=>1});

    $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
        $session, $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});

    my $reporter = new Blah($syncher);
    die "Failed" unless $syncher->setRemoteServer($rid, $reporter);

    $syncher->testOnRemote("set_up");
}

sub tear_down {
    my ($rid, $passive) = @_;
    if ($passive) {
        $syncher = new Foswiki::Plugins::ReadWriteOfflinePlugin::Syncher(
            $session, $Foswiki::cfg{Plugins}{ReadWriteOfflinePlugin}{LocalID});
        $syncher->getStore($remote_web)->remove();
        $syncher->getStore($remote_web2)->remove();
    } else {
        $syncher->getStore($local_web)->remove();
        $syncher->testOnRemote("tear_down");
    }
    $syncher->finish();
}

sub move {
    my ($passive) = @_;
    if ($passive) {
        Foswiki::Func::moveTopic(
            $remote_web, 'TestTopic1', $remote_web, 'TestTopicRenamed');
    } else {
        Foswiki::Func::moveTopic(
            $local_web, 'TestTopic1', $local_web, 'TestTopicRenamed');
    }
}

sub listContents {
    my ($web, $revs) = @_;
    my $ltn = $syncher->getContents($web);
    if ($revs) {
        return join(', ',map{"$_\@$ltn->{$_}"} sort keys %$ltn);
    } else {
        return join(', ',sort keys %$ltn);
    }
}

sub listRemoteContents {
    my ($syncher, $web, $revs) = @_;
    my $ltn = $syncher->getContentsFromRemote($web);
    if ($revs) {
        return join(', ',map{"$_\@$ltn->{$_}"} sort keys %$ltn);
    } else {
        return join(', ',sort keys %$ltn);
    }
}

sub listRow {
    my ($loc, $set, $reporter) = @_;
    return "<tr><th>$loc</th><td>".listContents($set, 1)."</td></tr>";
}

sub test_A_getEnv {
    my $syncher = shift;
    my $now = time();
    my $info = $syncher->getEnv();
    die 'lWiki version '.Data::Dumper->Dump([$info])
      unless $info->{'Foswiki::VERSION'} eq $Foswiki::VERSION;
    die 'lWiki release '.Data::Dumper->Dump([$info])
      unless $info->{'Foswiki::RELEASE'} eq $Foswiki::RELEASE;
    die 'lRWOP version '.Data::Dumper->Dump([$info])
      unless $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION'}
        eq $Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION;
    die 'lRWOP release '.Data::Dumper->Dump([$info])
      unless $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE'}
        eq $Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE;
    $info = $syncher->getEnvFromRemote();
    die 'rWiki dversion '.Data::Dumper->Dump([$info])
      unless defined $info->{'Foswiki::VERSION'};
    die 'rWiki drelease '.Data::Dumper->Dump([$info])
      unless defined $info->{'Foswiki::RELEASE'};
    die 'rRWOP dver '.Data::Dumper->Dump([$info]) unless
      defined $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION'};
    die 'rRWOP drel '.Data::Dumper->Dump([$info]) unless
      defined $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE'};
    if ($loopback) {
        die 'rWiki version '.Data::Dumper->Dump([$info])
          unless $info->{'Foswiki::VERSION'} eq $Foswiki::VERSION;
        die 'rWiki release '.Data::Dumper->Dump([$info])
          unless $info->{'Foswiki::RELEASE'} eq $Foswiki::RELEASE;
        die 'rRWOP ver '.Data::Dumper->Dump([$info])
          unless $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION'}
            eq $Foswiki::Plugins::ReadWriteOfflinePlugin::VERSION;
        die 'rRWOP rel '.Data::Dumper->Dump([$info])
          unless $info->{'Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE'}
            eq $Foswiki::Plugins::ReadWriteOfflinePlugin::RELEASE;
    }
    print("getEnv passed<hr>");
}

sub test_B_getFromRemote {
    my $syncher = shift;

    # Create remote TestTopic1 rev 1
    my @name = ($remote_web, 'TestTopic1', undef);
    my $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    # Create remote rev 2
    @name = ($remote_web, 'TestTopic1', undef);
    $rev = $syncher->putToRemote(\@name, "Rev2", 1);

    # Get rev 1 of $remote_web/TestTopic1, which is know to have 2 revs
    @name = ( $remote_web, 'TestTopic1', undef );
    my $content = $syncher->getFromRemote(\@name, 1);
    die $content unless $content =~ /Rev1/;

    # Get rev 2
    $content = $syncher->getFromRemote(\@name, 2);
    die $content unless $content =~ /Rev2/;

    # Try to get rev 3, which does not exist
    eval {
        $content = $syncher->getFromRemote(\@name, 3);
    };
    die $content unless $@;

    # now check an attachment. Get one from a topic known to be in the
    # standard installation. First get it direct from the filesystem
    # so we can compare content
    my $stdtopic = 'DocumentGraphics';
    open(F, "<",
         "$Foswiki::cfg{PubDir}/$Foswiki::cfg{SystemWebName}/$stdtopic/info.gif")
      || die "Could not open standard attachment: $!";
    local $/ = undef;
    binmode(F);
    my $expected = <F>;
    close(F);

    # Now get it using getFromRemote
    @name = ($Foswiki::cfg{SystemWebName}, $stdtopic, 'info.gif');
    $content = $syncher->getFromRemote(\@name);
    die "Not the same" unless $content eq $expected;

    # Get a topic from a subweb
    @name = ("$remote_web/SubWeb", 'TestTopic1', undef);
    $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    $content = $syncher->getFromRemote(\@name, 1);
    die $content unless $content =~ /Rev1/;

    ############# ERROR CONDITIONS #############

    # Get a topic that doesn't exist
    eval {
        @name = ($local_web, 'NonExistantTopic1');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # From a web that doesn't exist
    eval {
        @name = ('notarealwebhonestinjun', 'WebPreferences');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # Get an attachment that doesn't exist
    eval {
        @name = ($local_web, 'TestTopic1', 'bad_attitude.txt');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # undef name
    eval {
        @name = ();
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # illegal web name
    eval {
        @name = ('*(^&**^', '@:{{("', '!¬`|>');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # illegal topic name
    eval {
        @name = ($local_web, '@:{{("', '!¬`|>');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # illegal attachment name
    eval {
        @name = ($local_web, 'TestTopic1', '!¬`|>');
        $content = $syncher->getFromRemote();
    };
    die "BAD $content $@" unless $@;

    # Dot not legal as subweb specifier
    eval {
        @name = ( "$local_web.SubWeb", 'WebPreferences', undef );
        $content = $syncher->getFromRemote(\@name, 1);
    };
    die "BAD $content $@" unless $@;

    print("getFromRemote passed<hr>");
}

sub test_C_putToRemote {
    my $syncher = shift;

    # Create remote TestTopic2 rev 1
    my @name = ($remote_web, 'TestTopic2', undef);
    my $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    die $rev unless $rev == 1;
    my $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Rev1/;

    # Create remote rev 2
    @name = ($remote_web, 'TestTopic2', undef);
    $rev = $syncher->putToRemote(\@name, "Rev2", 1);
    die $rev unless $rev == 2;
    $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Rev2/;

    # Attachment rev 1
    @name = ($remote_web, 'TestTopic2', 'wibble.dat');
    $rev = $syncher->putToRemote(\@name, "Wibbl1", 0);
    die $rev unless $rev == 1;
    $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Wibbl1/;

    # Attachment rev 2
    $rev = $syncher->putToRemote(\@name, "Wurble", 1);
    die $rev unless $rev == 2;
    $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Wurble/;

    # Put to a web that doesn't exist on the other side
    # Should create the web, and rev 1 of the topic
    @name = ($remote_web2, 'WebPreferences', undef);
    $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    die unless $rev;
    $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Rev1/;

    # Again, attachment this time
    @name = ($remote_web2, 'TestTopic', 'Att1.dat');
    $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    die unless $rev;
    $content = $syncher->getFromRemote(\@name);
    die $content unless $content =~ /Rev1/;
    # create the webprefs so it's recognised as a web
    @name = ($remote_web2, 'WebPreferences', undef);
    $rev = $syncher->putToRemote(\@name, "Rev1", 1);

    # put to an illegal attachment name
    @name = ($remote_web, 'TestTopic', '*&^"£(*"$');
    eval {
        $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    };
    die "BAD" unless defined $@;

    # put to an illegal topic name
    @name = ($remote_web, '*&^"£(*"$');
    eval {
        $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    };
    die "BAD" unless $@;

    # Put to an illegal web name
    @name = ('*&^"£(*"$', 'OkName');
    eval {
        $rev = $syncher->putToRemote(\@name, "Rev1", 0);
    };
    die "BAD" unless $@;

    print("putToRemote passed<hr>");
}

sub _checkContents {
    my ($contents, $key, $val) = @_;
    die "undef $key (".join(',',keys %$contents).") at ".join(' ',caller)
      unless defined $contents->{$key};
    if (defined $val) {
        die join(' ',caller) unless $contents->{$key} == $val;
    }
    delete $contents->{$key};
    if (defined $contents->{"timestamp_$key"}) {
        delete $contents->{"timestamp_$key"};
    }
}

sub test_D_getContents {
    my $syncher = shift;

    my @list = Foswiki::Func::getTopicList($local_web);
    my $contents = $syncher->getContents($local_web);
    _checkContents($contents, 'T!TestTopic1', 2);
    _checkContents($contents, 'T!WebPreferences', 1);
    _checkContents($contents, 'W!SubWeb', 1);
    _checkContents($contents, 'A!TestTopic1.Att1.txt', 1);
    _checkContents($contents, 'T!ViewDeniedAll');
    _checkContents($contents, 'T!ViewDeniedOthers');
    _checkContents($contents, 'T!ChangeDeniedOthers');
    _checkContents($contents, 'T!ChangeDeniedAll');

    die join(',',keys %$contents) if scalar(keys %$contents);

    # Create remote TestTopic2 rev 1
    my @name = ($remote_web, 'TestTopic1', undef);
    $syncher->putToRemote(\@name, "Rev1", 0);
    # Create remote rev 2
    @name = ($remote_web, 'TestTopic1', undef);
    $syncher->putToRemote(\@name, "Rev2", 1);
    @name = ("$remote_web/SubWeb", 'TestTopic1', undef);
    $syncher->putToRemote(\@name, "Rev1", 0);

    $contents = $syncher->getContentsFromRemote($remote_web);
    _checkContents($contents, 'T!WebPreferences');
    _checkContents($contents, 'T!TestTopic1', 2);
    _checkContents($contents, 'W!SubWeb');
    die join(',',keys %$contents) if scalar(keys %$contents);

    # Contents of a non-existant web
    $contents = $syncher->getContentsFromRemote('notawebseriouslynodontargue');
    die join(',',keys %$contents) if scalar(keys %$contents);

    # Contents of an illegal webname
    eval {
        $contents = $syncher->getContentsFromRemote('*(&$^$');
    };
    die "BAD $contents $@" unless $@;

    print("getContents passed<hr>");
}

sub test_E_updateLocalFromRemote {
    my $syncher = shift;
    my @remote = ($remote_web, 'RemoteTopic', undef);
    $syncher->putToRemote(\@remote, "Rev1", 0);

    # First check that the new topic is brought over
    my @local = ($local_web, 'RemoteTopic', undef);
    my $lrev = $syncher->updateLocalFromRemote(\@local, 0, \@remote);
    die $lrev unless $lrev == 1;

    # Add another rev to remote_web so we know when it has been synched
    $syncher->putToRemote(\@remote, "Rev2", 1);

    # Check that the update happens
    $lrev = $syncher->updateLocalFromRemote(\@local, 1, \@remote);
    die $lrev unless $lrev == 2;

    # Make sure we got the new rev
    my $handler = $syncher->getStore($local_web, 'TestTopic1');
    $lrev = $handler->currentRevision();
    die $lrev unless $lrev == 2;
    my $contents = $handler->getRevision($lrev);
    die $contents unless $contents =~ /Rev2/;

    print("updateLocalFromRemote passed<hr>");
}

sub test_F_updateRemoteFromLocal {
    my $syncher = shift;

    # TestTopic1 does not exist in remote yet
    my @local = ($local_web, 'TestTopic1', undef);
    my @remote = ($remote_web, 'TestTopic1', undef);
    my $lrev = $syncher->updateRemoteFromLocal(\@local, \@remote, 0);
    die $lrev unless $lrev == 1;
    my $contents = $syncher->getFromRemote(\@remote);
    die $contents unless $contents =~ /Rev2/;
    $contents = $syncher->getContentsFromRemote($remote_web);
    _checkContents($contents, 'T!TestTopic1', 1);

    # Topic exists in both local and remote. Update it locally.
    Foswiki::Func::saveTopic(
        $local_web, "TestTopic1", undef, "Rev3");

    # Now update remote
    $lrev = $syncher->updateRemoteFromLocal(\@local, \@remote, 1);
    # There should be 2 remote revs
    die $lrev unless $lrev == 2;

    # Make sure we got the new rev
    $contents = $syncher->getContentsFromRemote($remote_web);
    _checkContents($contents, 'T!TestTopic1', 2);

    $contents = $syncher->getFromRemote(\@remote);
    die $contents unless $contents =~ /Rev3/;

    print("updateLocalFromRemote passed<hr>");
}

sub test_G_mergeRemoteAndLocal {
    my $syncher = shift;

    # Set up local_web with three topics
    my $handler = $syncher->getStore($local_web, 'MergeTopic1');
    $handler->addRevision("Merge topic 1", undef);
    $handler = $syncher->getStore($local_web, 'MergeTopic2');
    $handler->addRevision("Merge topic 2", undef);
    $handler->addRevision("r2\nMerge topic 2", undef);
    $handler = $syncher->getStore($local_web, 'MergeTopic3');
    $handler->addRevision("Merge topic 3", undef);

    # Now set up remote_web with the same three topic names
    my @rmt1 = ($remote_web, 'MergeTopic1');
    my $rev = $syncher->putToRemote(\@rmt1, 'Merge topic 1', 0);
    my @rmt2 = ($remote_web, 'MergeTopic2');
    $rev = $syncher->putToRemote(\@rmt2, 'Merge topic 2', 0);
    my @rmt3 = ($remote_web, 'MergeTopic3');
    $rev = $syncher->putToRemote(\@rmt3, 'Merge topic 3', 0);
    $rev = $syncher->putToRemote(\@rmt3, "r2\nMerge topic 3", 1);

    # We now have revs as follows:
    # |             | *local* | *remote* |
    # | MergeTopic1 |    1    |     1    |
    # | MergeTopic2 |    2    |     1    |
    # | MergeTopic3 |    1    |     2    |
    # Each with the same text (the start state)

    # Now add a new rev to each of these topics in local and remote
    $handler = $syncher->getStore($local_web, 'MergeTopic1');
    $handler->addRevision("Merge topic 1\ntext from test", undef);
    $handler = $syncher->getStore($local_web, 'MergeTopic2');
    $handler->addRevision("Merge topic 2\ntext from test", undef);
    $handler = $syncher->getStore($local_web, 'MergeTopic3');
    $handler->addRevision("Merge topic 3\ntext from test", undef);

    $syncher->putToRemote(\@rmt1, "Merge topic 1\ntext from other", 1);
    $syncher->putToRemote(\@rmt2, "Merge topic 2\ntext from other", 1);
    $syncher->putToRemote(\@rmt3, "Merge topic 3\ntext from other", 2);

    # Now we have:
    # |             | *local* | *remote* |
    # | MergeTopic1 |    2    |     2    |
    # | MergeTopic2 |    3    |     2    |
    # | MergeTopic3 |    2    |     3    |
    # All with *different* text in the two systems.

    # Now merge the first topic, local@2, remote@2, assuming last
    # synch was 1 <-> 1
    my @mt1 = ($local_web, 'MergeTopic1');
    my ($lrev, $rrev) =
      $syncher->mergeRemoteAndLocal(\@mt1, 1, 2, \@rmt1, 1, 2);
    die unless $lrev == 3;
    die unless $rrev == 3;
    $handler = $syncher->getStore($local_web, 'MergeTopic1');
    my $local = $handler->getRevision($lrev);
    my $remote = $syncher->getFromRemote(\@rmt1, $rrev);
    $local =~ s/^%META:TOPICINFO.*?\n//s;
    $remote =~ s/^%META:TOPICINFO.*?\n//s;
    die "Local\n'$local'\nRemote\n'$remote'\n" unless $local eq $remote;

    # Merge the next topic, local@3, remote@2, assuming last merge was
    # 2<->1
    my @mt2 = ($local_web, 'MergeTopic2');
    ($rrev, $lrev) =
      $syncher->mergeRemoteAndLocal(\@mt2, 2, 3, \@rmt2, 1, 2);
    die unless $rrev == 3;
    die unless $lrev == 4;
    $handler = $syncher->getStore($local_web, 'MergeTopic2');
    $local = $handler->getRevision($lrev);
    $remote = $syncher->getFromRemote(\@rmt2, $rrev);
    $local =~ s/^%META:TOPICINFO.*?\n//s;
    $remote =~ s/^%META:TOPICINFO.*?\n//s;
    die "Local\n'$local'\nRemote\n'$remote'\n" unless $local eq $remote;

    # Finally merge the third topic, local@2, remote@3, assuming last
    # synch was 1<->2
    my @mt3 = ($local_web, 'MergeTopic3');
    ($rrev, $lrev) =
      $syncher->mergeRemoteAndLocal(\@mt3, 1, 2, \@rmt3, 2, 3);
    die unless $rrev == 4;
    die unless $lrev == 3;
    $handler = $syncher->getStore($local_web, 'MergeTopic3');
    $local = $handler->getRevision($lrev);
    $local =~ s/%META.*?version="1.(\d+)".*?}%\n//;
    die unless $1 == $lrev;
    $remote = $syncher->getFromRemote(\@rmt3, $rrev);
    $local =~ s/^%META:TOPICINFO.*?\n//s;
    $remote =~ s/^%META:TOPICINFO.*?\n//s;
    die "Local\n'$local'\nRemote\n'$remote'\n" unless $local eq $remote;

    print("mergeRemoteAndLocal passed<hr>");
}

sub test_H_synchWeb {
    my $syncher = shift;

    # Synch the populated local_web with the empty remote_web.

    # Wait for the clock
    my $t = time();
    my $basetime;
    while (($basetime = time()) == $t) {
    }

    # Now do the synch. The topic lists should be identical afterwards
    my $reporter = new Blah($syncher);
    $syncher->synchWeb($local_web, $remote_web, $reporter);

    my $lcmp = listContents($local_web);

    $lcmp = listContents($local_web, 1);
    my $orig = $lcmp;
    die $lcmp unless $lcmp =~ s/timestamp_T!WebPreferences\@\d+//;
    die $lcmp unless $lcmp =~ s/T!WebPreferences\@2//;
    die $lcmp unless $lcmp =~ s/timestamp_T!TestTopic1\@\d+//;
    die $lcmp unless $lcmp =~ s/T!TestTopic1\@2//;
    die $lcmp unless $lcmp =~ s/timestamp_A!TestTopic1\.Att1\.txt\@\d+//;
    die $lcmp unless $lcmp =~ s/A!TestTopic1\.Att1\.txt\@1//;
    die $lcmp unless $lcmp =~ s/timestamp_T!ViewDeniedAll\@\d+//;
    die $lcmp unless $lcmp =~ s/T!ViewDeniedAll\@1//;
    die $lcmp unless $lcmp =~ s/W!SubWeb\@1//;
    die $lcmp unless $lcmp =~ s/timestamp_T!ViewDeniedOthers\@\d+//;
    die $lcmp unless $lcmp =~ s/T!ViewDeniedOthers\@1//;
    die $lcmp unless $lcmp =~ s/timestamp_T!ChangeDeniedOthers\@\d+//;
    die $lcmp unless $lcmp =~ s/T!ChangeDeniedOthers\@1//;
    die $lcmp unless $lcmp =~ s/timestamp_T!ChangeDeniedAll\@\d+//;
    die $lcmp unless $lcmp =~ s/T!ChangeDeniedAll\@1//;

    die "$lcmp ($orig" if $lcmp =~ /[^, ]/;
    $lcmp = listContents($local_web);
    my $rcmp = listRemoteContents($syncher, $remote_web);
    die "Inconsistent\n$lcmp\nis not\n$rcmp" unless $lcmp eq $rcmp;
    $lcmp = listContents("$local_web/SubWeb", 1);
    die $lcmp unless $lcmp =~ s/timestamp_T!WebPreferences\@\d+//;
    die $lcmp unless $lcmp =~ s/T!WebPreferences\@1//;
    die $lcmp if $lcmp =~ /[^, ]/;
    $lcmp = listContents("$local_web/SubWeb");
    $rcmp = listRemoteContents($syncher, "$remote_web/SubWeb");
    die "Inconsistent\n$lcmp\nis not\n$rcmp" unless $lcmp eq $rcmp;

    $reporter->report("synchWeb passed<hr>");

    # Re-run the synch. Nothing should change!
    $output = '';
    $syncher->synchWeb($local_web, $remote_web, $grabber);
    die $output if $output;
    $reporter->report("No changes check passed<hr>");
}

sub test_I_deleted {
    my $syncher = shift;

    # Scenario: topic deleted locally but still there on remote
    #
    # If the topic has been touched on remote, then need to
    # copy it down; otherwise need to delete it on remote.

    # First synch $local_web to $remote_web
    my $reporter = new Blah($syncher);
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    # Delete a topic remotely
    my @remote = ($remote_web, 'ChangeDeniedAll', undef);
    $syncher->deleteVersionedObjectFromRemote(\@remote, 1);

    # Delete a topic locally
    my @local = ($local_web, 'TestTopic1', undef);
    $syncher->logEvent('remove ', 'test', @local);
    $syncher->deleteVersionedObject(\@local, 2);

    # doze to let the clock tick over
    $reporter->report("Dozing...");
    sleep(1);

    # Now synch again. The topics should be deleted *on both sides*
    # (and so should the attachments!)
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    $reporter->report("Slobadob...");
    my $lcmp = listContents($local_web);
    $reporter->report($lcmp);
    die "Unwanted T!TestTopic1 in $lcmp" if $lcmp =~ s/T!TestTopic1//;
    die "Unwanted T!ChangeDeniedAll in $lcmp" if $lcmp =~ s/T!ChangeDeniedAll//;
    my $rcmp = listRemoteContents($syncher, $remote_web);
    die "Inconsistent\n$lcmp\nis not\n$rcmp" unless $lcmp eq $rcmp;
}

sub test_L_localRenameTopicWithAttachments {
    my $syncher = shift;
    # First synch $local_web to $remote_web
    my $reporter = new Blah($syncher);
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    #print "<h5>Finished first synch, renaming</h5>\n";

    # Rename local topic with attachments.
    move(0);

    # Re-synch
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    #print "<h5>Finished second synch</h5>\n";

    my $local = listContents($local_web);
    my $remote = listRemoteContents($syncher, $remote_web);
    die "<h4>Mismatch: Local</h4><pre>$local</pre>"
      ."<h4>Remote</h4><pre>$local</pre>" unless $local eq $remote;
    die "T!TestTopicRenamed missing"
      unless $local =~ /T!TestTopicRenamed/;
    die "A!TestTopicRenamed.Att1.txt missing"
      unless $local =~ /A!TestTopicRenamed.Att1.txt/;
    die "Unexpected T!TestTopic1"
      if $local =~ /T!TestTopic1/;
    die "Unexpected A!TestTopic1.Att1.txt"
      if $local =~ /A!TestTopic1.Att1.txt/;
}

sub test_M_remoteRenameTopicWithAttachments {
    my $syncher = shift;
    # First synch $local_web to $remote_web
    my $reporter = new Blah($syncher);
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    #print "<h5>Finished first synch, renaming</h5>\n";
    sleep(1);
    $syncher->testOnRemote("move");
    sleep(1);
    # Re-synch
    $syncher->synchWeb($local_web, $remote_web, $reporter);
    #print "<h5>Finished second synch</h5>\n";
    my $local = listContents($local_web);
    my $remote = listRemoteContents($syncher, $remote_web);
    die "<h4>Mismatch: Local</h4><pre>$local</pre>"
      ."<h4>Remote</h4><pre>$local</pre>" unless $local eq $remote;
    die "T!TestTopicRenamed missing from $local"
      unless $local =~ /T!TestTopicRenamed/;
    die "A!TestTopicRenamed.Att1.txt missing"
      unless $local =~ /A!TestTopicRenamed.Att1.txt/;
    die "Unexpected T!TestTopic1"
      if $local =~ /T!TestTopic1/;
    die "Unexpected A!TestTopic1.Att1.txt"
      if $local =~ /A!TestTopic1.Att1.txt/;
}

1;
