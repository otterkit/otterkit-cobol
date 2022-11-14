#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include "mpdecimal.h"

#ifdef _WIN32
#pragma comment(lib, "libmpdec-2.5.1.lib")
#define DLLEXPORT __declspec(dllexport)
#endif

#ifndef _WIN32
#define DLLEXPORT
#endif

// macOS & Linux Build commands
// Linux with GCC:
// gcc -I. -shared -Wl,-rpath libmpdec.so.3 -O2 -o Decimal128.so UnixDecimal128.c libmpdec.a -lm
//
// macOS with GCC:
// gcc -dynamiclib -Wall -W -O2 -o Decimal128.dylib UnixDecimal128.c libmpdec.a -lm
//
// macOS with Clang:
// cc -dynamiclib -Wall -W -O2 -o Decimal128.dylib UnixDecimal128.c libmpdec.a -lm

// Build command for windows
// cl.exe /D_USRDLL /D_WINDLL Decimal128.c /MT /Ox /link /DLL /OUT:Decimal128.dll

char stack[32][45];
char string[45];
int count = 0;

bool isOperator(char character)
{
	switch (character)
	{
		case '+':
			return true;

		case '-':
			return true;

		case '*':
			return true;

		case '/':
			return true;

		case '^':
			return true;

		default:
			return false;
	}
}

void stringAppend(char character)
{
	if (strlen(string) >= 44)
	{
		printf("\nOtterkit Buffer Overflow:\n");
		printf("Numeric string buffer overflow, ignoring all subsequent digits\n");
		return;
	}
	char temporary[2] = { character, '\0' };
	strcat(string, temporary);
}

void push()
{
	if (count + 1 >= 32)
	{
		printf("\nOtterkit Stack Overflow:\n");
		printf("Decimal arithmetic stack overflow, ignoring all subsequent operands\n");
		return;
	}
	strcpy(stack[count++], string);
	memset(string, 0, sizeof string);
}

char* pop()
{
	return stack[--count];
}


// Otterkit Postfix Arithmeric Calculator
// This function calls libmpdec for Decimal128 arithmetic
DLLEXPORT
char *OtterkitArithmetic(char *expression)
{
	// Declare libmpdec variables
	mpd_context_t context;
	mpd_t *left;
	mpd_t *right;
	mpd_t *result;

	// Result string
	char *rstring;

	// Initialize libmpdec's IEEE Decimal128 context
	mpd_ieee_context(&context, MPD_DECIMAL128);
	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);

	int index = 0;
	count = 0;
	// Evaluate postfix expression until end of string
	while (expression[index] != '\0')
	{
		char current = expression[index];
		char previous = expression[index - 1];
		char next = expression[index + 1];
		if (isdigit(current))
		{
			// Append character to string stack
			stringAppend(current);
		}

		if (current == '-' && isdigit(next))
		{
			// Append '-' to string stack for negative numbers
			stringAppend(current);
		}

		if (current == 'E' && (next == '-' || next == '+'))
		{
			// Append '-' to string stack for negative numbers
			stringAppend(current);
		}

		if (current == '.' && isdigit(previous) && isdigit(next))
		{
			// Append '.' to string stack for decimal values
			stringAppend(current);
		}

		if (current == ' ' && isdigit(previous))
		{
			// Push string stack values to the true stack
			push();
		}

		if (isOperator(current) && isdigit(next) == false)
		{
			switch (current)
			{
				case '+':
					// Pop two numbers from the stack
					// and add them together
					mpd_set_string(right, pop(), &context);
					mpd_set_string(left, pop(), &context);
					mpd_add(result, left, right, &context);
					break;

				case '-':
					// Pop two numbers from the stack
					// and subtract right from the left
					mpd_set_string(right, pop(), &context);
					mpd_set_string(left, pop(), &context);
					mpd_sub(result, left, right, &context);
					break;

				case '*':
					// Pop two numbers from the stack
					// and multiply left by the right
					mpd_set_string(right, pop(), &context);
					mpd_set_string(left, pop(), &context);
					mpd_mul(result, left, right, &context);
					break;

				case '/':
					// Pop two numbers from the stack
					// and divide left by the right
					mpd_set_string(right, pop(), &context);
					mpd_set_string(left, pop(), &context);
					mpd_div(result, left, right, &context);
					break;

				case '^':
					// Pop two numbers from the stack
					// and calculate left to the power of right
					mpd_set_string(right, pop(), &context);
					mpd_set_string(left, pop(), &context);
					mpd_pow(result, left, right, &context);
					break;

				default:
					break;
			}

			rstring = mpd_to_sci(result, 1);
			// Push result string to the string stack
			strcat(string, rstring);
			// Push string stack value to the true stack
			push();
		}
		index++;
	}

	// Final result
	rstring = mpd_to_sci(result, 1);
	
	// Free allocated memory
	mpd_del(right);
	mpd_del(left);
	mpd_del(result);
	
	return rstring;
}

DLLEXPORT
char *Decimal128Exp(char *exponent)
{
	mpd_context_t context;
	mpd_t *exp;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	exp = mpd_new(&context);
	
	mpd_set_string(exp, exponent, &context);

	mpd_exp(result, exp, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(exp);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Sqrt(char *value)
{
	mpd_context_t context;
	mpd_t *sqrt;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	sqrt = mpd_new(&context);
	
	mpd_set_string(sqrt, value, &context);

	mpd_sqrt(result, sqrt, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(sqrt);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Ln(char *value)
{
	mpd_context_t context;
	mpd_t *ln;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	ln = mpd_new(&context);
	
	mpd_set_string(ln, value, &context);

	mpd_ln(result, ln, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(ln);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Log10(char *value)
{
	mpd_context_t context;
	mpd_t *log10;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	log10 = mpd_new(&context);
	
	mpd_set_string(log10, value, &context);

	mpd_log10(result, log10, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(log10);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Abs(char *value)
{
	mpd_context_t context;
	mpd_t *abs;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	abs = mpd_new(&context);
	
	mpd_set_string(abs, value, &context);

	mpd_abs(result, abs, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(abs);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Plus(char *value)
{
	mpd_context_t context;
	mpd_t *plus;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	plus = mpd_new(&context);
	
	mpd_set_string(plus, value, &context);

	mpd_plus(result, plus, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(plus);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Minus(char *value)
{
	mpd_context_t context;
	mpd_t *minus;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	minus = mpd_new(&context);
	
	mpd_set_string(minus, value, &context);

	mpd_minus(result, minus, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(minus);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Rem(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_rem(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Compare(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_compare(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Max(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_max(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Min(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_min(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Shift(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_shift(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128And(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_and(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Or(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_or(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

DLLEXPORT
char *Decimal128Xor(char *value_left, char *value_right)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value_left, &context);
	mpd_set_string(right, value_right, &context);

	mpd_xor(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}
