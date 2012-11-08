/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

/**
 *
 * @author tianlize
 */
public class UpdateBeanReplace extends UpdateBean {

//    private String primaryValue;
    private String newValue;
    
    public UpdateBeanReplace(){
        super();
    }
    
    public UpdateBeanReplace(String obj, String attr, String op,String newVal){
        super(obj,attr,op);
//        this.primaryValue=primary;
        this.newValue=newVal;
    }
    
    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }

//    public String getPrimaryValue() {
//        return primaryValue;
//    }
//
//    public void setPrimaryValue(String primaryValue) {
//        this.primaryValue = primaryValue;
//    }
}
