class node:
    def __init__(self,char):
        self.char=char
        self.descendent={}
        self.isEnd=False
        
class trie:
    def __init__(self):
        self.root=node('')
 
    def add(self, word):
        curr_node=self.root
        for char in word:
            if char not in curr_node.descendent:
                curr_node.descendent[char]=node(char)
            curr_node=curr_node.descendent[char]
        curr_node.isEnd=True
        
    def include(self, word):
        curr_node=self.root
        for char in word:
            if char not in curr_node.descendent:
                return False
            curr_node=curr_node.descendent[char]
        return curr_node.isEnd
 
    def get_prefixes(self, word):
        prefix=''
        prefixes=[]
        curr_node=self.root
        for char in word:
            if char not in curr_node.descendent:
                return prefixes
            curr_node=curr_node.descendent[char]
            
            prefix=prefix+char
            if curr_node.isEnd:
                prefixes.append(prefix)
        return prefixes

def get_compound(words):
    trie_1=trie()
    queue=[]
    for word in words:
        prefixes=trie_1.get_prefixes(word)
        for prefix in prefixes:
            queue.append( (word, word[len(prefix):]) )
        trie_1.add(word)
 
    
    longestWord=[]
    maxLength=0
    
    while queue:
        word, suffix = queue.pop(0)
        if trie_1.include(suffix):
            print word
        else:
            prefixes=trie_1.get_prefixes(suffix)
            
            for prefix in prefixes:
                queue.append( (word, suffix[len(prefix):]) )
    