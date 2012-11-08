package it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces;

import java.io.Serializable;

public class ComplexReturnType implements Serializable {
    
    private String name;
    
    private int age;
    
    private ComplexReturnTypeAddress address;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public ComplexReturnTypeAddress getAddress() {
        return address;
    }

    public void setAddress(ComplexReturnTypeAddress address) {
        this.address = address;
    }
    
    

}
