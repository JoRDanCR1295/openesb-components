package it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces;

import java.io.Serializable;

public class ComplexReturnTypeAddress implements Serializable {
    
    public String street;
    
    public String city;

    public String getStreet() {
        return street;
    }

    public void setStreet(String street) {
        this.street = street;
    }

    public String getCity() {
        return city;
    }

    public void setCity(String city) {
        this.city = city;
    }
        

}
