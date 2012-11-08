/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

import com.sun.jbi.ldapbc.extensions.LDAPResponseAbstract;

/**
 *
 * @author tianlize
 */
public class LDAPUpdateResponse extends LDAPResponseAbstract{

    private String code;
    private String requestId="";
    private Boolean opResult=false;
    
    public LDAPUpdateResponse(){
        opResult=false;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public Boolean getOpResult() {
        return opResult;
    }

    public void setOpResult(Boolean opResult) {
        this.opResult = opResult;
    }

    public String getRequestId() {
        return requestId;
    }

    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }
}
