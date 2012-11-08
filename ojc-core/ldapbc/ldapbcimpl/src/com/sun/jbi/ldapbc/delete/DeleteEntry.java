/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.delete;

import java.util.ArrayList;
import java.util.List;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.LdapContext;

/**
 *
 * @author zhangwenbin
 */
public class DeleteEntry {

    private String dn;
    private List<DeleteEntry> childEntry = new ArrayList<DeleteEntry>();

    public DeleteEntry(String dn) {
        this.dn = dn;
    }

    public void deleteEntry(LdapContext ctx) {
        getChildEntry(ctx);
        deleteChildEntry(ctx);
        try {
            ctx.destroySubcontext(dn);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void getChildEntry(LdapContext ctx) {
        try {
            SearchControls constraints = new SearchControls();
            constraints.setSearchScope(SearchControls.ONELEVEL_SCOPE);
            NamingEnumeration results = ctx.search(dn, "(ObjectClass=*)", constraints);
            while (results != null && results.hasMore()) {
                SearchResult si = (SearchResult) results.next();
                String sdn = si.getNameInNamespace();
                childEntry.add(new DeleteEntry(sdn));
            }
        } catch (NamingException ex) {
            ex.printStackTrace();
        }
    }

    private void deleteChildEntry(LdapContext ctx) {
        if (childEntry.size() < 1) {
            return;
        }
        for (int i = 0; i < childEntry.size(); i++) {
            DeleteEntry entry = childEntry.get(i);
            entry.deleteEntry(ctx);
        }
    }

}
