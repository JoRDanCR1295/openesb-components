/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.update;

import com.sun.jbi.ldapbc.LDAPSearch;
import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 *
 * @author tianlize
 */
public class LDAPUpdate extends LDAPOperationAbstract{

    private LDAPSearch ldapSearch=new LDAPSearch();
    private List<UpdateBean> updateAttributes;

    public void addUpdateAttribute(UpdateBean bean){
        if(updateAttributes==null){
            updateAttributes=new ArrayList<UpdateBean>();
        }
        updateAttributes.add(bean);
    }
    
    public LDAPSearch getLdapSearch() {
        return ldapSearch;
    }

    public void setLdapSearch(LDAPSearch ldapSearch) {
        this.ldapSearch = ldapSearch;
    }

    public List<UpdateBean> getUpdateAttributes() {
        return updateAttributes;
    }

    public void setUpdateAttributes(List<UpdateBean> updateAttributes) {
        this.updateAttributes = updateAttributes;
    }
}
