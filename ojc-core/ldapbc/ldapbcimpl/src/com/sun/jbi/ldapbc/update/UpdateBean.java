/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

/**
 *
 * @author tianlize
 */
public class UpdateBean {
    
    public static final String OPERATION_TYPE_ADD = "Add";
    public static final String OPERATION_TYPE_REPLACE = "Replace";
    public static final String OPERATION_TYPE_REMOVE = "Remove";
    public static final String OPERATION_TYPE_REMOVEALL = "RemoveAll";

    private String objName;
    private String attrName;
    private String opType;

    public UpdateBean() {

    }

    public UpdateBean(String obj, String attr, String op) {
        this.objName = obj;
        this.attrName = attr;
        this.opType = op;
    }

    public String getAttrName() {
        return attrName;
    }

    public void setAttrName(String attrName) {
        this.attrName = attrName;
    }

    public String getObjName() {
        return objName;
    }

    public void setObjName(String objName) {
        this.objName = objName;
    }

    public String getOpType() {
        return opType;
    }

    public void setOpType(String opType) {
        this.opType = opType;
    }
}
