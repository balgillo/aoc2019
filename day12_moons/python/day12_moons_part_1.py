import re
import sys

def sign(d):
    return -1 if d < 0 else 1 if d > 0 else 0


def abs(xyz):
    return sum((c if c > 0 else -c) for c in xyz)


class Body:
    def __init__(self, position):
        self.position = position
        self.velocity = (0, 0, 0)


    def acceleration_due_to_gravity(self, other_body):
        return tuple(sign(other_body.position[i] - self.position[i]) for i in range(3))


    def apply_gravity(self, other_bodies):
        for other_body in other_bodies:
            acc = self.acceleration_due_to_gravity(other_body)
            self.velocity = tuple(self.velocity[i] + acc[i] for i in range(3))
    

    def tick(self):
        self.position = tuple(self.position[i] + self.velocity[i] for i in range(3))


    def total_energy(self):
        potential_energy = abs(self.position)
        kinetic_energy = abs(self.velocity)
        return potential_energy * kinetic_energy


    def __str__(self):
        return "pos=<x={0: >3}, y={1: >3}, z={2: >3}>, vel=<x={3: >3}, y={4: >3}, z={5: >3}>".format(
            self.position[0], self.position[1], self.position[2],
            self.velocity[0], self.velocity[1], self.velocity[2]
        )


def load_moons(file_path):
    moons = []
    LINE_REGEX = r"<x=([\s+\-]?[0-9]+),\s*y=([\s+\-]?[0-9]+),\s*z=([\s+\-]?[0-9]+)>"
    with open(file_path) as f:
        for l in f.readlines():
            m = re.match(LINE_REGEX, l)
            x = int(m[1])
            y = int(m[2])
            z = int(m[3])
            moons.append(Body((x, y, z)))
    return moons



moons = load_moons(sys.argv[1])

total_steps = int(sys.argv[2])

for step in range(0, total_steps + 1):
    print("After step {}:".format(step))
    total_energy = 0
    for moon in moons:
        total_energy += moon.total_energy()
        print(moon)
    print("Total energy: {}".format(total_energy))
    print()
    for moon in moons:
        moon.apply_gravity(moons)
    for moon in moons:
        moon.tick()
