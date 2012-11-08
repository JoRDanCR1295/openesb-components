/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

/**
 *
 * @author tianlize
 */
public class UpdateBeanRemove extends UpdateBean{

    private String removeValue;

    public UpdateBeanRemove(){
        super();
    }
    
    public UpdateBeanRemove(String obj, String attr, String op,String remove){
        super(obj,attr,op);
        this.removeValue=remove;
    }
    
    public String getRemoveValue() {
        return removeValue;
    }

    public void setRemoveValue(String removeValue) {
        this.removeValue = removeValue;
    }
}
