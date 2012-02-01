class CBaseClass {
  public:
  STRING* ClassName;
  STRING* ClassParentName;
  ~CBaseClass();
};

CBaseClass::~CBaseClass()
{

}

void delete(CBaseClass* ClassPointer)
{
	ClassPointer.Destructor();
	sys_free(ClassPointer);
}
