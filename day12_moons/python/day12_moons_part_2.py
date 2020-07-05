import math
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


class OneDimensionalManyBodySimulator:

    class State:
        def __init__(self, step, positions, velocities):
            self.step = step
            self.positions = positions
            self.velocities = velocities


        def next(self):
            next_step = self.step + 1
            next_positions = []
            next_velocities = []
            body_count = len(self.positions)
            for i in range(body_count):
                p = self.positions[i]
                v = self.velocities[i] + sum((1 if p2 > p else -1 if p2 < p else 0) for p2 in self.positions)
                next_velocities.append(v)
                next_positions.append(p + v)
            return OneDimensionalManyBodySimulator.State(next_step, next_positions, next_velocities)


        def total_energy(self):
            ret = 0
            body_count = len(self.positions)
            for i in range(body_count):
                e = self.positions[i] * self.velocities[i]
                ret += -e if e < 0 else e
            return ret

        
        def is_identical(self, other_state):
            return self.positions == other_state.positions and self.velocities == other_state.velocities


    def __init__(self, positions, velocities):
        self.initial_state = OneDimensionalManyBodySimulator.State(0, positions, velocities)
        self.history = {}


    def calc_repeat_period(self):
        state = self.initial_state
        while True:
            e = state.total_energy()
            if e in self.history:
                historic_states = self.history[e]
                for historic_state in historic_states:
                    if historic_state.is_identical(state):
                        return state.step - historic_state.step
                historic_states.append(state)
            else:
                self.history[e] = [state]
            state = state.next()


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

repeat_period_for_all_dimensions = 1
for dimension in range(0, 3):
    s1d = OneDimensionalManyBodySimulator([m.position[dimension] for m in moons], [m.velocity[dimension] for m in moons])
    repeat_period_for_this_dimension = s1d.calc_repeat_period()
    # Calculate least common multiple
    repeat_period_for_all_dimensions *= repeat_period_for_this_dimension // math.gcd(repeat_period_for_all_dimensions, repeat_period_for_this_dimension)

print(repeat_period_for_all_dimensions)
