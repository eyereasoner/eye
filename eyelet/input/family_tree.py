"""
family_tree.py — a self‑contained family tree implementation in Python
"""

class Person:
    def __init__(self, name, gender):
        self.name = name
        self.gender = gender
        self.parents = set()
        self.children = set()
        self.spouse = None

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
        if not person: return []
        return sorted(person.children, key=lambda x: x.name)

    def get_siblings(self, name):
        person = self.people.get(name)
        if not person or not person.parents:
            return []

        siblings = set()
        for parent in person.parents:
            siblings.update(parent.children)
        siblings.discard(person)
        return sorted(siblings, key=lambda x: x.name)

    def get_brothers(self, name):
        brothers = [s for s in self.get_siblings(name) if s.gender == 'male']
        return sorted(brothers, key=lambda x: x.name)

    def get_sisters(self, name):
        sisters = [s for s in self.get_siblings(name) if s.gender == 'female']
        return sorted(sisters, key=lambda x: x.name)

    def get_grandparents(self, name):
        person = self.people.get(name)
        if not person: return []

        grandparents = set()
        for parent in person.parents:
            grandparents.update(parent.parents)
        return sorted(grandparents, key=lambda x: x.name)

    def get_grandfathers(self, name):
        grandfathers = [g for g in self.get_grandparents(name) if g.gender == 'male']
        return sorted(grandfathers, key=lambda x: x.name)

    def get_grandmothers(self, name):
        grandmothers = [g for g in self.get_grandparents(name) if g.gender == 'female']
        return sorted(grandmothers, key=lambda x: x.name)

    def get_uncles(self, name):
        person = self.people.get(name)
        if not person: return []

        uncles = set()
        # Parent's brothers (blood uncles)
        for parent in person.parents:
            uncles.update(self.get_brothers(parent.name))

        # Spouses of parent's sisters (aunts by marriage)
        for parent in person.parents:
            for aunt in self.get_sisters(parent.name):
                if aunt.spouse and aunt.spouse.gender == 'male':
                    uncles.add(aunt.spouse)

        return sorted(uncles, key=lambda x: x.name)

    def get_aunts(self, name):
        person = self.people.get(name)
        if not person: return []

        aunts = set()
        # Parent's sisters (blood aunts)
        for parent in person.parents:
            aunts.update(self.get_sisters(parent.name))

        # Spouses of parent's brothers (uncles by marriage)
        for parent in person.parents:
            for uncle in self.get_brothers(parent.name):
                if uncle.spouse and uncle.spouse.gender == 'female':
                    aunts.add(uncle.spouse)

        return sorted(aunts, key=lambda x: x.name)

    def add_spouse(self, name1, name2):
        person1 = self.people.get(name1)
        person2 = self.people.get(name2)
        if not person1 or not person2:
            raise ValueError("One or both persons not found")

        if person1.gender == person2.gender:
            raise ValueError("Spouses must be different genders")

        person1.spouse = person2
        person2.spouse = person1

    def __str__(self):
        return f"FamilyTree with {len(self.people)} members"

# Example usage with spouse relationships
if __name__ == "__main__":
    family = FamilyTree()

    # Add family members
    family.add_person("Grandpa", "male")
    family.add_person("Grandma", "female")
    family.add_person("Dad", "male")
    family.add_person("Mom", "female")
    family.add_person("Child1", "male")
    family.add_person("Child2", "female")
    family.add_person("Uncle", "male")        # Dad's brother
    family.add_person("Aunt", "female")       # Uncle's wife
    family.add_person("Cousin", "male")
    family.add_person("MomSister", "female")  # Mom's sister

    # Add spouses
    family.add_spouse("Grandpa", "Grandma")
    family.add_spouse("Dad", "Mom")
    family.add_spouse("Uncle", "Aunt")

    # Establish relationships
    family.add_relationship("Grandpa", "Dad")
    family.add_relationship("Grandma", "Dad")
    family.add_relationship("Grandpa", "Uncle")
    family.add_relationship("Grandma", "Uncle")
    family.add_relationship("Dad", "Child1")
    family.add_relationship("Mom", "Child1")
    family.add_relationship("Dad", "Child2")
    family.add_relationship("Mom", "Child2")
    family.add_relationship("Uncle", "Cousin")
    family.add_relationship("Aunt", "Cousin")

    # Add maternal side
    family.add_person("MaternalGrandpa", "male")
    family.add_person("MaternalGrandma", "female")
    family.add_relationship("MaternalGrandpa", "Mom")
    family.add_relationship("MaternalGrandma", "Mom")
    family.add_relationship("MaternalGrandpa", "MomSister")
    family.add_relationship("MaternalGrandma", "MomSister")

    # Query relationships
    print(f"Father of Child1: {family.get_father('Child1')}")
    print(f"Mother of Child2: {family.get_mother('Child2')}")
    print(f"Children of Dad: {[c.name for c in family.get_children('Dad')]}")
    print(f"Siblings of Child1: {[s.name for s in family.get_siblings('Child1')]}")
    print(f"Brothers of Child2: {[b.name for b in family.get_brothers('Child2')]}")
    print(f"Grandparents of Child1: {[g.name for g in family.get_grandparents('Child1')]}")
    print(f"Uncles of Child1: {[u.name for u in family.get_uncles('Child1')]}")
    print(f"Aunts of Child2: {[a.name for a in family.get_aunts('Child2')]}")

