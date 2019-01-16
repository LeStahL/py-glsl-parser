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

# AST Node base class
class ASTNode:
    def __init__(self):
        self.children = []
        self.parent = None
        pass
    
    def __init__(self, parent):
        self.children = []
        self.parent = parent
        pass
    
    def hasParent(self):
        return self.parent == None
    
    def parent(self):
        return self.parent
    
    def setParent(self, parent):
        self.parent = parent
        pass
    
    def children(self):
        return self.children
    
    def appendChild(self, child):
        self.children += [ child ]
        pass
    
    def hasChildren(self):
        return self.children.count() == 0
    
    def removeChild(self, index):
        return self.children.pop(index)
    
    def depth(self):
        obj = self
        d = 0
        while obj.hasParent():
            d += 1
            obj = obj.parent()
        return d
    
    def toString(self):
        d = self.depth()
        buf = ' ' * d + '{'
        for c in self.children:
            buf += c.toString()
        buf += ' ' * d + '}'
        return buf
    
    def root(self):
        if self.hasParent():
            return self.parent()
        return self

