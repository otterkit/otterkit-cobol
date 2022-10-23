using System.Numerics;

namespace OtterkitLibrary;

public class OtterkitTypes
{
    public class Numeric
    {
        private Decimal128 internalNumber;
        private int integerLength;
        private int fractionalLength;
        private bool isNegative = false;
        private bool isSigned = false;

        public Numeric(Decimal128 number, int integerLength, int fractionalLength, bool signed)
        {
            this.internalNumber = number;
            this.integerLength = integerLength;
            this.fractionalLength = fractionalLength;
            this.isSigned = signed;
            if (number < 0 && isSigned)
            {
                isNegative = true;
            }
        }

        private string Formatter(Decimal128 number)
        {
            if (fractionalLength != 0)
            {
                // TODO: This looks like spaghetti, refactor later
                string[] numericString = number.ToString().Split(".");
                string padInteger = numericString[0].PadLeft(integerLength, '0');
                string padFraction = numericString[1].PadRight(fractionalLength, '0');
                padInteger = padInteger.Substring(padInteger.Length - integerLength, padInteger.Length);
                padFraction = padFraction.Substring(0, fractionalLength);
                return string.Concat(padInteger, ".", padFraction);

            }
            // TODO: This one as well, refactor later
            return number.ToString().PadLeft(integerLength, '0').Substring(number.ToString().Length - integerLength, number.ToString().Length);
        }

        public Decimal128 Value
        {
            get
            {
                return (isNegative && isSigned)
                        ? -internalNumber
                        : internalNumber;
            }
            set
            {
                if (value < 0 && isSigned)
                {
                    isNegative = true;
                }

                if (value >= 0 && isSigned)
                {
                    isNegative = false;
                }

                internalNumber = value;
            }
        }

        public string DisplayValue
        {
            get
            {
                if (isNegative && isSigned)
                {
                    return String.Format("-{0}", Formatter(internalNumber));
                }

                if (!isNegative && isSigned)
                {
                    return String.Format("+{0}", Formatter(internalNumber));
                }

                return Formatter(internalNumber);
            }
        }
    }

    public class Alphanumeric
    {
        private string internalString;
        private int stringSize;

        public Alphanumeric(string str, int size)
        {
            this.stringSize = size;
            this.internalString = str.PadRight(stringSize).Substring(0, stringSize);
        }

        public string Value
        {
            get
            {
                return internalString;
            }
            set
            {
                internalString = value.PadRight(stringSize).Substring(0, stringSize);
            }
        }

    }

    public class Alphabetic
    {
        private string internalString;
        private int stringSize;

        public Alphabetic(string str, int size)
        {
            if (str.Any(char.IsDigit))
            {
                throw new ArgumentException("Alphabetic type cannot contain numeric values", str);
            }
            this.stringSize = size;
            this.internalString = str.PadRight(stringSize).Substring(0, stringSize);
        }

        public string Value
        {
            get
            {
                return internalString;
            }
            set
            {
                if (value.Any(char.IsDigit))
                {
                    throw new ArgumentException("Alphabetic type cannot contain numeric values", value);
                }
                internalString = value.PadRight(stringSize).Substring(0, stringSize);
            }
        }

    }
}