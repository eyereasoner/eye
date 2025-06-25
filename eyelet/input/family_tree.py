"""
family_tree.py — a self‑contained family tree implementation in Python
"""

class Person:
    def __init__(self, name, gender):
        self.name = name
        self.gender = gender
        self.parents = set()
        self.children = set()

    def __repr__(self):
        return self.name

class FamilyTree:
    def __init__(self):
        self.people = {}
    
    def add_person(self, name, gender):
        if name in self.people:
            raise ValueError(f"{name} already exists in family tree")
        self.people[name] = Person(name, gender)
    
    def add_relationship(self, parent_name, child_name):
        if parent_name not in self.people:
            raise ValueError(f"Parent {parent_name} not found")
        if child_name not in self.people:
            raise ValueError(f"Child {child_name} not found")
        
        parent = self.people[parent_name]
        child = self.people[child_name]
        
        if parent == child:
            raise ValueError("Cannot be parent of self")
        if child in self.get_ancestors(parent):
            raise ValueError("Relationship creates cycle")
        
        child.parents.add(parent)
        parent.children.add(child)
    
    def get_ancestors(self, person):
        ancestors = set()
        stack = [person]
        while stack:
            current = stack.pop()
            for parent in current.parents:
                if parent not in ancestors:
                    ancestors.add(parent)
                    stack.append(parent)
        return ancestors
    
    def get_father(self, name):
        person = self.people.get(name)
        if not person: return None
        return next((p for p in person.parents if p.gender == 'male'), None)
    
    def get_mother(self, name):
        person = self.people.get(name)
        if not person: return None
        return next((p for p in person.parents if p.gender == 'female'), None)
    
    def get_children(self, name):
        person = self.people.get(name)
        return list(person.children) if person else []
    
    def get_siblings(self, name):
        person = self.people.get(name)
        if not person or not person.parents: 
            return []
        
        siblings = set()
        for parent in person.parents:
            for child in parent.children:
                if child != person:
                    siblings.add(child)
        return list(siblings)
    
    def get_brothers(self, name):
        return [s for s in self.get_siblings(name) if s.gender == 'male']
    
    def get_sisters(self, name):
        return [s for s in self.get_siblings(name) if s.gender == 'female']
    
    def get_grandparents(self, name):
        person = self.people.get(name)
        if not person: return []
        
        grandparents = set()
        for parent in person.parents:
            grandparents.update(parent.parents)
        return list(grandparents)
    
    def get_grandfathers(self, name):
        return [g for g in self.get_grandparents(name) if g.gender == 'male']
    
    def get_grandmothers(self, name):
        return [g for g in self.get_grandparents(name) if g.gender == 'female']
    
    def get_uncles(self, name):
        person = self.people.get(name)
        if not person: return []
        
        uncles = set()
        for parent in person.parents:
            for grandparent in parent.parents:
                for uncle_aunt in grandparent.children:
                    if uncle_aunt != parent and uncle_aunt.gender == 'male':
                        uncles.add(uncle_aunt)
        return list(uncles)
    
    def get_aunts(self, name):
        person = self.people.get(name)
        if not person: return []
        
        aunts = set()
        for parent in person.parents:
            for grandparent in parent.parents:
                for uncle_aunt in grandparent.children:
                    if uncle_aunt != parent and uncle_aunt.gender == 'female':
                        aunts.add(uncle_aunt)
        return list(aunts)
    
    def __str__(self):
        return f"FamilyTree with {len(self.people)} members"

# Example usage
if __name__ == "__main__":
    family = FamilyTree()
    
    # Add ALL family members first
    family.add_person("Grandpa", "male")
    family.add_person("Grandma", "female")
    family.add_person("Dad", "male")
    family.add_person("Mom", "female")
    family.add_person("Child1", "male")
    family.add_person("Child2", "female")
    family.add_person("Uncle", "male")
    family.add_person("Aunt", "female")
    family.add_person("Cousin", "male")  # Added missing person
    
    # Establish relationships
    family.add_relationship("Grandpa", "Dad")
    family.add_relationship("Grandma", "Dad")
    family.add_relationship("Grandpa", "Uncle")
    family.add_relationship("Grandma", "Uncle")
    family.add_relationship("Dad", "Child1")
    family.add_relationship("Mom", "Child1")
    family.add_relationship("Dad", "Child2")
    family.add_relationship("Mom", "Child2")
    family.add_relationship("Uncle", "Cousin")  # Fixed typo (was "Uncle")
    
    # Query relationships
    print(f"Father of Child1: {family.get_father('Child1')}")
    print(f"Mother of Child2: {family.get_mother('Child2')}")
    print(f"Children of Dad: {family.get_children('Dad')}")
    print(f"Siblings of Child1: {family.get_siblings('Child1')}")
    print(f"Brothers of Child2: {family.get_brothers('Child2')}")
    print(f"Grandparents of Child1: {family.get_grandparents('Child1')}")
    print(f"Uncles of Child1: {family.get_uncles('Child1')}")
    print(f"Aunts of Child2: {family.get_aunts('Child2')}")

