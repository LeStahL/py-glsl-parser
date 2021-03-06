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

from ASTNode import *

# AST Node for Identifiers
class VariableIdentifier(ASTNode):
    def __init__(self):
        super(VariableIdentifier, self).__init__()
        self.name = ""
        pass
    
    def __init__(self, parent):
        super(VariableIdentifier, self).__init__(parent=parent)
        self.name = ""
        pass

    def __init__(self, parent, name):
        super(VariableIdentifier, self).__init__(parent=parent)
        self.name = name
        pass
    
    def toString(self):
        return self.name
    
