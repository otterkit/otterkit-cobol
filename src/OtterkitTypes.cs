using System.Numerics;
using OtterkitLibrary.Numerics;

namespace OtterkitLibrary;

public class OtterkitTypes
{
    public class Numeric
    {
        private BigDecimal internalNumber;
        private int intSize;
        private int intSizePow10;
        private int decSize;
        private bool isNegative = false;
        private bool isSigned = false;

        public Numeric(BigDecimal number, int intSize, int decSize, bool signed)
        {
            this.internalNumber = number;
            this.intSize = intSize;
            this.intSizePow10 = (int)Math.Pow(10, intSize);
            this.decSize = decSize + 1;
            this.isSigned = signed;
            if (number < 0 && isSigned)
            {
                isNegative = true;
            }
        }

        private BigDecimal Truncate(BigDecimal number)
        {
            string IntValue = (number.Truncate() % intSizePow10).ToString("decimal");
            string DecValue = (number - number.Truncate()).ToString("decimal");
            return String.Format("{0}{1}", IntValue, DecValue.Substring(0, decSize));
        }
        private string Formatter(BigDecimal number)
        {
            string IntValue = (number.Truncate() % intSizePow10).ToString("decimal").PadLeft(4, '0');
            string DecValue = (number - number.Truncate()).ToString("decimal").PadLeft(decSize, '0');
            return String.Format("{0}{1}", IntValue, DecValue.Substring(0, decSize));
        }

        public BigDecimal Value
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