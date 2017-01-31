all : test

test : parameters-test agent-test subscription-test

parameters-test :
	PYTHONPATH=.. python parameters-test.py

agent-test :
	PYTHONPATH=.. python agent-test.py

subscription-test :
	PYTHONPATH=.. python subscription-test.py
