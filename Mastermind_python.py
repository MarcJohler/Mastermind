# -*- coding: utf-8 -*-
"""
Created on Wed Feb 22 14:39:13 2023

@author: Marc Johler
"""
import numpy as np
import tkinter as tk

def assert_natural_number(x):
    assert isinstance(x, str) or isinstance(x, int)
    
    assert x > 0
    
def assert_code(x, code_length, allow_duplicates):
    assert isinstance(x, str)
    # check code length
    assert len(x) == code_length
    # check if there are duplicates
    if not allow_duplicates:
        assert len(x) == len(set(x))
    # check if all digits are valid
    digits = set("0123456789")
    assert digits.issuperset(set(x))

class GameLog(object):
    def __init__(self, code_length, max_turns):
        assert_natural_number(code_length)
        assert_natural_number(max_turns)
        
        self._max_turns = max_turns
        self._code_length = code_length
        self.guesses = []
        self.positions_correct = []
        self.digits_correct = []
        self.remaining_turns = max_turns
    
    def save_guess(self, guess, pos_correct, dig_correct):
        self.guesses.append(guess)
        self.positions_correct.append(pos_correct)
        self.digits_correct.append(dig_correct)
        self.remaining_turns -= 1
        
# create root windows for tkinter
ROOT = tk.Tk()
ROOT.withdraw()
        
class Game(object):
    def generate_code(self):
        digits = [i for i in range(10)]
        code = np.random.choice(digits, self._code_length, self._allow_duplicates)
        self.code = ""
        for digit in code:
            self.code += str(digit)
    
    def __init__(self, code_length, allow_duplicates, max_turns, code = None, secret_message = None):
        assert_natural_number(code_length)
        assert_natural_number(max_turns)
        # if there are no duplicates allowed check if code length is restricted
        if not allow_duplicates:
            assert code_length <= 10
        self._code_length = code_length
        self._allow_duplicates = allow_duplicates
        # if code is not given generate it
        if code is None:
            self.generate_code()
        # otherwise check if code is admissible
        else:
            # convert code to string
            if isinstance(code, int):
                code = str(code)
            assert_code(code, code_length)  
            self.code = code
        
        self._max_turns = max_turns
        self._secret_message = secret_message
        self._game_log = GameLog(code_length, max_turns)
        
    def guess(self, guess):
        assert_code(guess, self._code_length, self._allow_duplicates)
        # compute number of digits on correct positions
        pos_matches = [self.code[i] == guess[i] for i in range(len(guess))]
        pos_correct = np.sum(pos_matches)
        # compute number of correct digits on wrong positions
        remaining_code = []
        remaining_guess = []
        for i in range(len(pos_matches)):
            # if the positions does not match, consider for next step
            if not pos_matches[i]:
                remaining_code.append(self.code[i])
                remaining_guess.append(guess[i])
        # compare the digits
        dig_correct = 0
        for digit in remaining_guess:
            i = 0
            while i < len(remaining_code):
                if digit == remaining_code[i]:
                    dig_correct += 1
                    remaining_code.pop(i)
                    break
                i += 1
        # save the guess in the log
        self._game_log.save_guess(guess, pos_correct, dig_correct)
        # check if the game was won
        if pos_correct == self._code_length:
            return True
        return False
                    
    def start_game(self):
        while True:
            dialog_message = "Previous guesses: "
            for i in range(len(self._game_log.guesses)):
                first_part = "\n" + self._game_log.guesses[i] + " contains "
                second_part = str(self._game_log.positions_correct[i]) + " digits on correct positions and " 
                third_part = str(self._game_log.digits_correct[i]) + " correct digits on wrong positions."
                dialog_message += first_part + second_part + third_part
                
            dialog_message += "\n Tries left: " + str(self._game_log.remaining_turns)
                
            guess = None
            code_admissible = False
            while not code_admissible:
                code_admissible = True
                guess = tk.simpledialog.askstring(title='Insert code', prompt=dialog_message)
                # developer exit
                if guess == "EMERGENCY EXIT":
                    return None
                # otherwise try if code is admissible
                try: 
                    assert_code(guess, self._code_length, self._allow_duplicates)
                except: 
                    tk.messagebox.showinfo(title="Code info", message = "Please insert a valid code!")
                    code_admissible = False
            
            won = self.guess(guess)
            # check code
            if won:
                tk.messagebox.showinfo(title="Code info", message = "Code accepted!")
                tk.messagebox.showinfo(title="Success", message = self._secret_message)
                break
            else:
                tk.messagebox.showinfo(title="Code info", message = "Code declined!")
                # check if there are turns remaining
                if self._game_log.remaining_turns == 0:
                    tk.messagebox.showinfo(title="Code info", message = "No more tries left. New code will be generated automatically for security reasons")
                    # generate new code
                    self.generate_code()
                    # reset remaining turns
                    self._game_log.remaining_turns = self._max_turns
                    
game = Game(4, True, 10, secret_message = "The third digit for the case is 9")
game.start_game()