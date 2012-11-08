/*
 * LDAPSearch.java
 *
 * Created on April 29, 2007, 7:48 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;
import com.sun.jbi.ldapbc.util.LdapConnection;
import java.util.ArrayList;
import java.util.List;
import javax.naming.directory.SearchControls;

/**
 *
 * @author 
 */
public class LDAPSearch extends LDAPOperationAbstract{
    private String requestId;
    private String dn;
    private SearchControls control;
    private List attrs;
    private LdapConnection conn;
    /** Creates a new instance of LDAPSearch */
    public LDAPSearch() {
    }

    public void addFilter(int posIndex, String objName, String logicOp, String attrName,
            String compareOp, int bDepth, int bBeginDepth, int bEndDepth, String value) {
        Filter filter = new Filter(posIndex, objName, logicOp, attrName, compareOp, bDepth, bBeginDepth, bEndDepth, value);
        if (attrs == null) {
            attrs = new ArrayList();
        }

        attrs.add(filter);
    }

    public void setLdapConnection(LdapConnection conn) {
        this.conn = conn;
    }
    
    public LdapConnection getLdapConnection() {
        return conn;
    }
    
    public void setSearchControls(SearchControls control) {
        this.control = control;
    }
    
    public SearchControls getSearchControls() {
        return control;
    }
    
    public void setDN(String d) {
        dn = d;
    }

    public void setRequestId(String r) {
        requestId = r;
    }

    public List getAttributes() {
        return attrs;
    }

    public String getDN() {
        return dn;
    }

    public String getRequestId() {
        return requestId;
    }
}
