import argparse
import sys
from copy import deepcopy
from functools import reduce
from json import dumps
from itertools import product  # Import itertools for product

# Output file.
f = None

class SyntacticTree(object):
    """ Syntactic tree stores informations about symbols, either terminals
        or variables, in a hierarchical fashion. """
    def __init__(self, elem, terminal=True):
        self.elem = elem
        self.terminal = terminal

    def expand_and_output(self, symtable, max_depth, current_depth=0):
        global f
        # Check if current depth exceeds max depth
        if current_depth > max_depth:
            return []
        # If the node is a terminal, return its string representation
        if self.terminal:
            return [self.elem]
        else:
            all_strings = []
            for production in symtable.get(self.elem, []):
                child_options = []
                for symbol in production:
                    child_node = SyntacticTree(symbol['token'], symbol['terminal'])
                    child_strings = child_node.expand_and_output(symtable, max_depth, current_depth + 1)
                    if not child_strings:
                        break
                    child_options.append(child_strings)
                else:
                    # All children expanded successfully
                    for combination in product(*child_options):
                        full_string = ''.join(combination)
                        all_strings.append(full_string)
                        if current_depth == 0:
                            if full_string.strip():
                                f.write(full_string + '\n')
            return all_strings

def gen_tokens(s):
    return [{'token': e, 'terminal': not e.isupper()} for e in s.split(' ') if e]

def parse_grammar(fpath):
    # Read file.
    with open(fpath) as f:
        lines = f.readlines()
    # Break strings into tokens list.
    table = {}
    for line in lines:
        l = line.strip().split(':')
        if len(l) == 2:
            var = l[0].strip()
            prods = [prod.strip() for prod in l[1].split('|')]
            prods_tokens = [gen_tokens(prod) for prod in prods]
            table.setdefault(var, []).extend(prods_tokens)
    return table

def check_arg(args=None):
    parser = argparse.ArgumentParser(description='Generate all strings given a grammar within bounded syntax tree.')
    parser.add_argument('-f', '--file', help='Grammar file', required=False, default='basic.gr')
    parser.add_argument('-d', '--depth', help='Syntax tree max depth', required=False, default=3)
    parser.add_argument('-o', '--output', help='Output file', required=False, default='formulas.txt')
    results = parser.parse_args(args)
    return (results.file, int(results.depth), results.output)

def gen_strings(symtable, max_depth):
    tree = SyntacticTree('S', False)
    tree.expand_and_output(symtable, max_depth)

if __name__ == '__main__':
    fpath, depth, output = check_arg(sys.argv[1:])
    symtable = parse_grammar(fpath)
    f = open(output, 'w')
    gen_strings(symtable, depth)
    f.close()
