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
class Comment(ASTNode):
    def __init__(self):
        super(Comment, self).__init__()
        self.text = ""
        pass
    
    def __init__(self, parent):
        super(Comment, self).__init__(parent=parent)
        self.text = ""
        pass

    def __init__(self, parent, text):
        super(Comment, self).__init__(parent=parent)
        self.text = text
        pass
    
    def toString(self):
        return "/*" + self.text + "*/"
    
