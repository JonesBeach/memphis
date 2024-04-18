def something():
    return 5

def something_else():
    return 6

def something_third():
    return 7

def something_fourth():
    return 8

def second_call():
    return something_else()

def kwargs_call(**kwargs):
    return kwargs['a']

class OtherClass:
    def run(self):
        return something_fourth()
