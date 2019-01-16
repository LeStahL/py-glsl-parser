# py-glsl-parser - Python GLSL Parser
#
# Copyright (C) 2018  Alexander Kraus <nr4@z10.info>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

from Parser130 import *

import argparse

# Parse command line arguments
parser = argparse.ArgumentParser(description='py-glsl-parser test.')
args, rest = parser.parse_known_args()

# Exit if inconsistent argument combinations are supplied
if rest == []:
    print("No input present. Doing nothing. Type 'shader_minifier -h' for help.")
    exit()

string = ""
with open(rest[0], 'rt') as f:
    string = f.read()
    f.close()

AST = parse(string)
print(AST.toString())
