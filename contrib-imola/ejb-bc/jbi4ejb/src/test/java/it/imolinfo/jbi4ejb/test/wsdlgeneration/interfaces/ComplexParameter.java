package it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces;

import java.io.Serializable;

public class ComplexParameter implements Serializable {

    int code;
    
    Long longCode;

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public Long getLongCode() {
        return longCode;
    }

    public void setLongCode(Long longCode) {
        this.longCode = longCode;
    }
    
    
}
