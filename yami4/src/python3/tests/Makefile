all : test

test : parameters-test agent-test subscription-test

parameters-test :
	PYTHONPATH=.. python3 parameters-test.py

agent-test :
	PYTHONPATH=.. python3 agent-test.py

subscription-test :
	PYTHONPATH=.. python3 subscription-test.py
