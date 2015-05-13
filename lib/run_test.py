#!/usr/bin/env python3
"""
run_test.py
AUTHOR : yoshimoto

libディレクトリ下の.lmnファイルについて、
/** ... */ 形式のコメントに含まれる {@test ... } 形式のテストコードを実行し、
結果を集計して標準出力に書き出す

テストコードの形式
  {@test process1 @==> process2}
  (ライブラリのuse, process1)を評価すれば、process2になる
  process2は、ルールを含まないLMNtalプロセスでなければならない
  現状の実装では、部分的にprocess2の構造ができるような実行経路が
  ひとつでもあれば、テストにパスする
"""

import uuid
import os.path
import re
from glob import glob
import textwrap
from subprocess import Popen, PIPE
import argparse

slim_path = '../src/slim'
compiler_path = os.path.expandvars('$LMNTAL_HOME/lmntal.jar')

def random_rule_name():
    return 'rule_' + uuid.uuid4().hex

def matches(regex, str):
    """
    strの先頭から、正規表現オブジェクトregexを検索してゆき、
    そのマッチオブジェクトをジェネレートする
    >>> [m.group() for m in matches(re.compile('a[^b]*b'), 'xaabxxaxbb')]
    ['aab', 'axb']
    """
    pos = 0
    while True:
        match = regex.search(str, pos)
        if not match:
            return
        pos = match.end()
        yield match

# 投げやりな実装
# ちゃんとやりたければコンパイラのパーサと協調すべき
doc_regex = re.compile(
    r'/\*\*[^\n]*\n(?P<lines>(\s*\*[^\n]*\n)*(\s*\*[^\n]*)?)\s\*/',
    re.MULTILINE)
def docs(filename):
    """
    filenameを開き、すべてのドキュメンテーションをイテレートする
    例えば、以下のようなドキュメンテーションは
    /**
     * Ret = func1:
     * This is a test function.
     * {@test
     *  ret = func1. @==> ret = nil.}
     */
    以下の文字列で表現される
    " Ret = func1:\n This is a test function.\n" +
    " {@test\n  ret = func1. @==> ret = nil.}"
    """
    with open(filename) as f:
        content = f.read()
    for doc_match in matches(doc_regex, content):
        lines = doc_match.group('lines').splitlines()
        yield '\n'.join(re.match('\s\*(.*)', line).group(1) for line in lines)

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument('lmntal',
                        help='path to lmntal compiler runner script (not jar)')
arg_parser.add_argument('slim',
                        help='path to slim')

if __name__ == '__main__':
    args = arg_parser.parse_args()
    lmntal_path = args.lmntal
    slim_path = args.slim

    '''
    for filename in glob('*.lmn'):
        print(filename)
    '''

    # LMNtalプログラムの文字列の中に
    # バランスしていない波括弧が含まれるとまずい
    test_regex = re.compile(r'\{@test\s(?P<test>[^{}]*(\{[^{}]*\}[^{}]*)*)\}')
    for doc in docs('functional.lmn'):
        for test_match in matches(test_regex, doc):
            test = test_match.group('test')
            print('running test:')
            print(test)

            lhs, rhs = (s.strip() for s in test.split('@==>'))

            # test
            # lhs, rhs = 'a', 'b'

            goal_rule_name = random_rule_name()
            # @testの中のLMNtalコードの右辺は、.で終わってはいけない
            test_code = textwrap.dedent('''\
            functional.use.
            {}.
            {} @@ {} :- .
            ''').format(lhs, goal_rule_name, rhs)
            # print(test_code)

            compiler = Popen([lmntal_path, '--stdin-lmn', '--slimcode', '-O3'],
                             stdin=PIPE, stdout=PIPE, stderr=PIPE,
                             universal_newlines=True)
            out, err = compiler.communicate(test_code)
            if compiler.returncode != 0:
                print('test failed')
                print(err)
                continue

            slim = Popen([slim_path, '--nd', '-t', '--show-transition',
                          '--hide-ruleset', '--use-builtin-rule', '-'],
                         stdin=PIPE, stdout=PIPE, stderr=PIPE,
                         universal_newlines=True)
            out, err = slim.communicate(out)
            if slim.returncode != 0:
                print('test failed')
                print(err)
                continue

            if goal_rule_name in out:
                print('test pass')
                # print(out)
            else:
                print('test failed')
                # print(out)
