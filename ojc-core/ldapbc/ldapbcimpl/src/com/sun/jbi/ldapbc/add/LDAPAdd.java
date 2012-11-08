/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.add;

import com.sun.jbi.ldapbc.extensions.LDAPOperationAbstract;
import com.sun.jbi.ldapbc.util.LdapConnection;
import javax.naming.directory.Attributes;

/**
 *
 * @author zhangwenbin
 */
public class LDAPAdd extends LDAPOperationAbstract {

    private String requestId;
    private String dn;
    private String rDN;
    private Attributes attrs;
    private String name;
    private LdapConnection conn;

    public LDAPAdd() {

    }

    public void setLdapConnection(LdapConnection conn) {
        this.conn = conn;
    }

    public LdapConnection getLdapConnection() {
        return conn;
    }
  public void setRDN(String rDN) {
       this.rDN = rDN;
    }
  
    public String getRDN() {
        return rDN;
    }
    public void setDN(String d) {
        dn = d;
    }

    public void setRequestId(String r) {
        requestId = r;
    }

    public String getDN() {
        return dn;
    }

    public String getRequestId() {
        return requestId;
    }

    public void setAttributes(Attributes attrs) {
        this.attrs = attrs;
    }

    public Attributes getAttributes() {
        return attrs;
    }

    public void setEntryName(String name) {
        this.name = name;
    }

    public String getEntryName() {
        return name;
    }
}
   
