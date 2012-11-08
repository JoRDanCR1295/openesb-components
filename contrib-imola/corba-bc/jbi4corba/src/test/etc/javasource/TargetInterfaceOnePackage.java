package it.imolinfo.freestyle;

import freestyle.FakeClassOnePackage;
import java.util.*;

public interface TargetInterfaceOnePackage extends SuperTargetInterface {

    public abstract Integer echoInterger();

    public String    echoCharString(char charParam, String stringParam);
    public FakeClassOnePackage echoFake(freestyle.FakeClassOnePackage[][] fake);

    public int       echoIntDouble(int intParam, double doubleParam);

    public void echoVoid();
    public void echoVoid2();
    public int echoInt2();
}
//name=echoInterger; ret=Integer
//name=echoCharString; ret=String
//name=echoFake; ret=FakeClass
//name=echoIntDouble; ret=int
//name=echoVoid; ret=void
//name=echoVoid2; ret=void
//name=echoInt2; ret=int
