namespace OtterkitLibrary;

public static class DPDEncoding
{
    static uint Encode(uint integer)
    {
        uint DPD = LUTS.DPDLUT[integer];
        return DPD;
    }

    static uint Decode(uint DPD)
    {
        uint integer = (uint)Array.IndexOf(LUTS.DPDLUT, DPD);
        return integer;
    }  
}