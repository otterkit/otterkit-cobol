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
        public Numeric(decimal number, string format, int intSize, int decSize)
        {
            if (number < 0) { isNegative = true; }
            this.internalNumber = number;
            this.integerSize = (int)Math.Pow(10, intSize);
            this.decimalSize = decSize;
            this.format = format;
        }
        private string Formatter(decimal number)
        {
            decimal IntValue = Math.Truncate(Math.Abs(number) % integerSize);
            decimal DecValue = Math.Round(Math.Abs(number), decimalSize, MidpointRounding.ToZero)
                                - Math.Truncate(Math.Abs(number));

            return (IntValue + DecValue).ToString(format);
        }
        private decimal Truncate(decimal number)
        {
            decimal IntValue = Math.Truncate(Math.Abs(number) % integerSize);
            decimal DecValue = Math.Round(Math.Abs(number), decimalSize, MidpointRounding.ToZero)
                                - Math.Truncate(Math.Abs(number));

            return IntValue + DecValue;
        }
        public decimal Value
        {
            get { return isNegative ? -Truncate(internalNumber) : Truncate(internalNumber); }
            set
            {
                if (value < 0) { isNegative = true; }
                internalNumber = value;
            }
        }
        public string DisplayValue
        {
            get
            {
                if (isNegative)
                {
                    return String.Format("-{0}", Formatter(internalNumber));
                }
                return Formatter(internalNumber);
            }
        }
    }
    public class Alphanumeric
    {

    }
}