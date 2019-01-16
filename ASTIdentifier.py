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

import ASTNode

# AST Node for Identifiers
class ASTIdentifier(ASTNode):
    def __init__(self):
        super(ASTIdentifier).__init__()
        self.name = ""
        pass
    
    def __init__(self, parent)
        super(ASTIdentifier).__init__(parent)
        self.name = ""
        pass

    def __init__(self, parent, name):
        __init__(self, parent)
        self.name = name
        pass
    
    def toString(self):
        return ' ' * self.depth + self.name
    