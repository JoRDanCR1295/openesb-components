/*
 * LDAPSearch.java
 *
 * Created on April 29, 2007, 7:48 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.ldapbc.extensions.LDAPResponseAbstract;
import java.util.ArrayList;
import java.util.List;
import javax.naming.NamingEnumeration;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.SearchResult;

/**
 *
 * @author Gary
 */
public class LDAPResponse extends LDAPResponseAbstract{

    private String code;
    private String requestId="";
    private List entries = new ArrayList();

    /** Creates a new instance of LDAPSearch */
    public LDAPResponse() {
    }

    public class Entry {

        private SearchResult result;

        public Entry(SearchResult sr) {
            result = sr;
        }

        public SearchResult getResult() {
            return result;
        }
    }

//    public String toString() {
//        String ret = "";
//        if (entries != null) {
//            for (int i = 0; i < entries.size(); i++) {
//                SearchResult result = ((Entry) entries.get(i)).getResult();
//                Attributes attrs = result.getAttributes();
//                NamingEnumeration ne = attrs.getAll();
//                Attribute a = null;
//                try {
//                    while (ne.hasMore()) {
//                        a = (Attribute) ne.next();
//                        ret+=a.toString()+"\n";
//                    }
//                } catch (Exception e) {
//                    e.printStackTrace();
//                } finally {
//                    ne = null;
//                    a = null;
//                }
//                ret+="*******************\n";
//            }
//        }else{
//            ret+="no result";
//        }
//        return ret;
//    }

    public void addEntry(SearchResult result) {
        Entry entry = new Entry(result);
        entries.add(entry);
    }

    public void setCode(String c) {
        code = c;
    }

    public void setRequestId(String r) {
        requestId = r;
    }

    public List getEntries() {
        return entries;
    }

    public String getCode() {
        return code;
    }

    public String getRequestId() {
        return requestId;
    }
}
