import argparse
import sys
from copy import *
from functools import reduce
from json import dumps

# Output file.
f = None

# Syntactic tree is a tree whose nodes are either terminals or non-terminals.
#
# Terminals are essentially strings and are defined on the right-side,
# lower-case of each production.
#
# Non-terminals, however, are defined on the left-side of each rule and each rule
# is separated by '|'.
class SyntacticTree(object):
  """ Syntatic tree stores informations about symbols, either terminals
      or variables, in an hierarchical fashion. """
  def __init__(self, elem, terminal=True):
    self.elem = elem
    self.terminal = terminal
    self.children = []
  def to_str(self, include_vars=False):
    if include_vars:
      return self.elem if len(self.children) == 0 else reduce(lambda x, y: x + y.to_str(True), self.children, '')
    return self.elem if self.terminal else reduce(lambda x, y: x + y.to_str(), self.children, '')
  def iscomplete(self):
    if len(self.children) == 0:
      return self.terminal
    for child in self.children:
      if not child.iscomplete():
        return False
    return True
  def find_nonterminal_leafs(self, path, write_out, symtable):
    if len(self.children) == 0 and not self.terminal:# and len(path) > 0:
      write_out.append({'path': path, 'symbol': self.elem})
    for i in range(len(self.children)):
      self.children[i].find_nonterminal_leafs(path + [i], write_out, symtable)
  def replace_children(self, production):
    self.children = [ SyntacticTree(symbol['token'], symbol['terminal']) for symbol in production ]
    return self
  def extend(self, symtable, directions, nums_rules, n):
    # Bijective map from [0..(num_trees-1)] into proper leafs' derivations'
    # choices, (v1, v2, ..., v_{len(directions)}),
    #
    #   v_i = floor(n / prod(s_j, j = i+1..V)) % s_i,    i = 1, 2, ..., V
    #
    # where
    #   
    #   v_i: index of leaf i
    #   V: number of leafs, len(directions)
    #   s_j: number of possible direct derivations at leaf i, nums_rules[i].
    #
    V = len(directions)
    v = list(map(lambda i: (n // reduce(lambda acc, j: acc * j, nums_rules[i+1:], 1)) % nums_rules[i], range(V)))
    # When expansion is at its last stage (leaf_distance=0), only expansions that generate complete trees take place.
    #if leaf_distance == 0 and reduce(lambda a, b: a or b, map(lambda i: directions[i]['symbol'].isupper(), range(V)), False):
    #  return self
    #
    # As long as the well-formed tuple v = (v1, v2, ..., v_V) is concerned,
    # the tree will expand leafs through derivations pointed by v.
    for i in range(V):
      self.append_child(directions[i]['path'], v[i], symtable)
    # Return the tree itself with expanded leafs.
    return self
  def append_child(self, direction, rule, symtable):
    if len(direction) > 0:
      return self.children[direction[0]].append_child(direction[1:], rule, symtable)
    self.children = [ SyntacticTree(symbol['token'], symbol['terminal']) for symbol in symtable[self.elem][rule] ]
    return self
  def delete_child(self, direction):
    if len(direction) > 0:
      return self.children[direction[0]].delete_child(direction[1:])
    self.children = []
    return self
  def extend_and_print(self, symtable, directions, nums_rules, num_trees):
    global f
    V = len(directions)
    for n in range(num_trees):
      v = list(map(lambda i: (n // reduce(lambda acc, j: acc * j, nums_rules[i+1:], 1)) % nums_rules[i], range(V)))
      # Append children accordingly and print them.
      valid = True
      i = 0
      while valid and i < V:
        if not self.append_child(directions[i]['path'], v[i], symtable).terminal:
          valid = False
        i += 1
      if valid:
        f.write(self.to_str() + "\n")
      # Clear children.
      for i in range(V):
        self.delete_child(directions[i]['path'])
      return self

# Given a syntactic tree and a depth, generate all possible strings wherein syntactic tree is bounded by 'depth'.
def expand_tree(tree, symtable):
  # Find directions of each non-terminal leaf, which should be expanded.
  directions = []
  tree.find_nonterminal_leafs([], directions, symtable)
  # Root expansion.
  if directions == [{'path': [], 'symbol': 'S'}]:
    return [ deepcopy(tree).replace_children(symtable['S'][i]) for i in range(len(symtable['S'])) ]
  else:
    num_trees = reduce(lambda x, y: x*y, list(map(lambda x: len(symtable[x['symbol']]), directions)), 1)
    num_leafs = len(directions)
    nums_rules = list(map(lambda x: len(symtable[x['symbol']]), directions))
    trees = [ deepcopy(tree).extend(symtable, directions, nums_rules, i) for i in range(num_trees) ]
    return trees

def gen_strings(symtable, depth, l):
  # Generate `depth-1'-long trees
  for d in range(1, depth):
    new_list = []
    for elem in l:
      new_list.extend(expand_tree(elem, symtable))
    l = new_list
  # Generate and print leafs
  for tree in l:
    directions = []
    tree.find_nonterminal_leafs([], directions, symtable)
    num_trees = reduce(lambda x, y: x*y, list(map(lambda x: len(symtable[x['symbol']]), directions)), 1)
    nums_rules = list(map(lambda x: len(symtable[x['symbol']]), directions))
    tree.extend_and_print(symtable, directions, nums_rules, num_trees)

# Break a string into tokens.
def gen_tokens(s):
  return [{'token': e, 'terminal': not e.isupper()} for e in s.split(' ')]

# Parse given grammar file path into a symbol table, a hash of list of productions.
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
      prods = list(map(gen_tokens, map(lambda s: s.strip(), l[1].split('|'))))
      for prod in prods:
        if var in table:
          table[var].append(prod)
        else:
          table[var] = [prod]
  return table

# Parse arguments as provided in POSIX-style.
def check_arg(args=None):
  parser = argparse.ArgumentParser(description='Generate all strings given a grammar within bounded syntax tree.')
  parser.add_argument('-f', '--file', help='Grammar file', required=False, default='basic.gr')
  parser.add_argument('-d', '--depth', help='Syntax tree max depth', required=False, default=3)
  parser.add_argument('-o', '--output', help='Output file', required=False, default='formulas.txt')
  results = parser.parse_args(args)
  return (results.file, int(results.depth), results.output)

# Debug symbol table.
def debug(obj):
  print(dumps(obj, sort_keys=True, indent=2))

# Main program, which will be executed when loaded not as a library.
if __name__ == '__main__':
  fpath, depth, output = check_arg(sys.argv[1:])
  symtable = parse_grammar(fpath)
  f = open(output, 'w')
  gen_strings(symtable, depth, [SyntacticTree('S', False)])
  f.close()
