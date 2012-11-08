/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.add;

import com.sun.jbi.ldapbc.extensions.LDAPResponseAbstract;
import javax.naming.directory.DirContext;

/**
 *
 * @author zhangwenbin
 */
public class LDAPAddResponse extends LDAPResponseAbstract {

    private String code;
    private String requestId = "";
    private Boolean opResult = false;
    private DirContext responseDirContext;

    public DirContext getResponseDirContext() {
        return responseDirContext;
    }

    public void setResponseDirContext(DirContext responseDirContext) {
        this.responseDirContext = responseDirContext;
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
