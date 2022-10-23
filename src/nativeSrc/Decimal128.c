#include <stdio.h>
#include <stdlib.h>
#include <mpdecimal.h>

char * Decimal128Pow(char *value, char *exponent)
{
	mpd_context_t context;
	mpd_t *left, *right;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	left = mpd_new(&context);
	right = mpd_new(&context);
	
	mpd_set_string(left, value, &context);
	mpd_set_string(right, exponent, &context);

	mpd_pow(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128Exp(char *exponent)
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

char * Decimal128Sqrt(char *value)
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

char * Decimal128Ln(char *value)
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

char * Decimal128Log10(char *value)
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

char * Decimal128Abs(char *value)
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

char * Decimal128Plus(char *value)
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

char * Decimal128Minus(char *value)
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

char * Decimal128Add(char *value_left, char *value_right)
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

	mpd_add(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128Sub(char *value_left, char *value_right)
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

	mpd_sub(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128Div(char *value_left, char *value_right)
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

	mpd_div(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128Mul(char *value_left, char *value_right)
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

	mpd_mul(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128Rem(char *value_left, char *value_right)
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

char * Decimal128Compare(char *value_left, char *value_right)
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

char * Decimal128Max(char *value_left, char *value_right)
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

char * Decimal128Min(char *value_left, char *value_right)
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

char * Decimal128Shift(char *value_left, char *value_right)
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

char * Decimal128Rotate(char *value_left, char *value_right)
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

	mpd_rotate(result, left, right, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(left);
	mpd_del(right);
	mpd_del(result);

	return string;
}

char * Decimal128And(char *value_left, char *value_right)
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

char * Decimal128Or(char *value_left, char *value_right)
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

char * Decimal128Xor(char *value_left, char *value_right)
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

char * Decimal128Invert(char *value)
{
	mpd_context_t context;
	mpd_t *inverted;
	mpd_t *result;
	char *string;

	mpd_ieee_context(&context, MPD_DECIMAL128);

	result = mpd_new(&context);
	inverted = mpd_new(&context);
	
	mpd_set_string(inverted, value, &context);

	mpd_invert(result, inverted, &context);

	string = mpd_to_sci(result, 1);

	mpd_del(inverted);
	mpd_del(result);

	return string;
}