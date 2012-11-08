/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.delete;

import com.sun.jbi.ldapbc.LDAPSearch;
import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;

/**
 *
 * @author zhangwenbin
 */
public class LDAPDelete extends LDAPOperationAbstract {

    private LDAPSearch ldapSearch = new LDAPSearch();

    public LDAPSearch getLdapSearch() {
        return ldapSearch;
    }

    public void setLdapSearch(LDAPSearch ldapSearch) {
        this.ldapSearch = ldapSearch;
    }
}
