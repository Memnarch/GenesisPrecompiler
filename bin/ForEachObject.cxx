class CForEachObject
{
private:
    int LF;
public:
    virtual void FEInit();
    virtual int FEHasMoreElements();
    virtual void* FENextElement();
};

