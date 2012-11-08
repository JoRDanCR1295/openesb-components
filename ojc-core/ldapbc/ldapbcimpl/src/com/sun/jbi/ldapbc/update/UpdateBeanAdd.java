/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

/**
 *
 * @author tianlize
 */
public class UpdateBeanAdd extends UpdateBean{

    private String addValue;
    
    public UpdateBeanAdd(){
        super();
    }
    
    public UpdateBeanAdd(String obj, String attr, String op,String add){
        super(obj,attr,op);
        this.addValue=add;
    }

    public String getAddValue() {
        return addValue;
    }

    public void setAddValue(String addValue) {
        this.addValue = addValue;
    }
}
