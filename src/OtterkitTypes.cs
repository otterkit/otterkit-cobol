namespace OtterkitLibrary;
public class OtterkitTypes
{
    public class Numeric
    {
        private decimal internalNumber;
        private int integerSize;
        private int decimalSize;
        private string format;
        private bool isNegative = false;
        private bool isSigned = false;

        public Numeric(decimal number, string format, int intSize, int decSize, bool signed)
        {
            this.internalNumber = number;
            this.format = format;
            this.integerSize = (int)Math.Pow(10, intSize);
            this.decimalSize = decSize;
            this.isSigned = signed;
            if (number < 0 && isSigned)
            {
                isNegative = true;
            }
        }

        private decimal Truncate(decimal number)
        {
            decimal IntValue = Math.Truncate(Math.Abs(number) % integerSize);
            decimal DecValue = Math.Round(Math.Abs(number), decimalSize, MidpointRounding.ToZero)
                                - Math.Truncate(Math.Abs(number));

            return IntValue + DecValue;
        }

        private string Formatter(decimal number)
        {
            return Truncate(number).ToString(format);
        }

        public decimal Value
        {
            get
            {
                return (isNegative && isSigned)
                        ? -Truncate(internalNumber)
                        : Truncate(internalNumber);
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