use inc::Module::Install;

name     'Convert-Moji';
all_from 'lib/Convert/Moji.pm';
author   'Ben Kasmin Bullock <benkasminbullock@gmail.com>';

build_requires 'Test::More';

auto_install;

WriteAll;

