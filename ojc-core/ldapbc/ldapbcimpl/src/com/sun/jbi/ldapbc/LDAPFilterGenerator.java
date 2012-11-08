/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.jbi.messaging.MessagingException;

/**
 *
 * @author tianlize
 */
public class LDAPFilterGenerator {

    List attrs;

    public LDAPFilterGenerator(List attrs) {
        this.attrs = attrs;
    }

    private Filter getAttrByPosIndex(int posIndex) {
        Filter ret = null;
        Iterator it = attrs.iterator();
        while (it.hasNext()) {
            Filter filter = (Filter) it.next();
            if (filter.getPositionIndex() == posIndex) {
                ret = new Filter();
                ret.setAttributeName(filter.getAttributeName());
                ret.setBracketBeginDepth(filter.getBracketBeginDepth());
                ret.setBracketDepth(filter.getBracketDepth());
                ret.setBracketEndDepth(filter.getBracketEndDepth());
                ret.setCompareOp(filter.getCompareOp());
                ret.setLogicOp(filter.getLogicOp());
                ret.setObjName(filter.getObjName());
                ret.setPositionIndex(posIndex);
                ret.setValue(filter.getValue());
            }
            filter = null;
        }
        return ret;
    }

    private List getSortedAttrs() throws MessagingException{
        List ret = new ArrayList();
        if (attrs != null) {
            for (int i = 0; i < attrs.size(); i++) {
                Filter filter = getAttrByPosIndex(i);
                if (filter != null) {
                    ret.add(i, filter);
                }
                filter = null;
            }
        }else{
           throw new MessagingException("LDAP attrs is null");
        }
        return ret;
    }

    private String singleAttrFilter(Filter filter) {
        String ret = "(";
        if (filter != null) {
            ret += filter.getAttributeName();
            ret += filter.getCompareOp();
            ret += filter.getValue();
        }
        ret += ")";
        return ret;
    }

    private String doubleAttrFilter(Filter f1, Filter f2) {
        String ret = "(";
        if (f1 != null & f2 != null) {
            ret += formatLogicOp(f2.getLogicOp());
            ret += singleAttrFilter(f1);
            ret += singleAttrFilter(f2);
        }
        ret += ")";
        return ret;
    }

    private int findOrIndex(List list, int begin, int end) {
        int ret = -1;
        for (int i = end; i >= begin; i--) {
            Filter filter = (Filter) list.get(i);
            if (filter.getLogicOp().equals("Or") & (filter.getBracketDepth() == 0 | filter.getBracketDepth() - filter.getBracketBeginDepth() == 0)) {
                ret = i;
                break;
            }
        }
        return ret;
    }

    private void removeBracket(List list, int beginIndex, int endIndex) {
        Filter fBegin = (Filter) list.get(beginIndex);
        Filter fEnd = (Filter) list.get(endIndex);
        fBegin.reduceBracketBeginDepth();
        fEnd.reduceBracketEndDepth();
        for (int i = beginIndex; i <= endIndex; i++) {
            Filter f = (Filter) list.get(i);
            f.reduceBracketDepth();
            f = null;
        }
        fBegin = null;
        fEnd = null;
    }

    private int getBracketBeginIndex(List list, Filter f, int beginIndex) {
        int ret = f.getPositionIndex();
        int bracketEndDepth = f.getBracketEndDepth();
        for (int i = ret - 1; i >= beginIndex; i--) {
            Filter f2 = (Filter) list.get(i);
            if (!f2.isBeginBracket()) {
                bracketEndDepth += f2.getBracketEndDepth();
                continue;
            }
            if (bracketEndDepth - f2.getBracketBeginDepth() <= 0) {
                ret = f2.getPositionIndex();
                break;
            }
            bracketEndDepth -= f2.getBracketBeginDepth();
            f2 = null;
        }
        return ret;
    }

    private String getLoginOp(List list, int index) {
        if (index < 0) {
            return "";
        }
        Filter f = (Filter) list.get(index);
        return f.getLogicOp();
    }

    private String formatLogicOp(String logicOp) {
        if (logicOp.equals("And")) {
            return "&";
        }
        if (logicOp.equals("Or")) {
            return "|";
        }
        return null;
    }

    private String getFilter(List list, int beginIndex, int endIndex) {
        if (beginIndex < 0 | endIndex < beginIndex | endIndex < 0) {
            return "";
        }
        String ret = "(";
        Filter filter = (Filter) list.get(endIndex);
        if (beginIndex == endIndex) {
            return singleAttrFilter(filter);
        }
        if (beginIndex + 1 == endIndex) {
            Filter filter2 = (Filter) list.get(beginIndex);
            return doubleAttrFilter(filter2, filter);
        }

        int j = findOrIndex(list, beginIndex, endIndex);
        if (j > beginIndex) {
            ret += formatLogicOp(getLoginOp(list, j)) + getFilter(list, beginIndex, j - 1) + getFilter(list, j, endIndex);
        } else {
            if (!filter.isEndBracket()) {
                ret += formatLogicOp(filter.getLogicOp()) + getFilter(list, beginIndex, endIndex - 1) + singleAttrFilter(filter);
            } else {
                int i = getBracketBeginIndex(list, filter, beginIndex);
                removeBracket(list, i, endIndex);
                if (i > beginIndex) {
                    ret += formatLogicOp(getLoginOp(list, i)) + getFilter(list, beginIndex, i - 1) + getFilter(list, i, endIndex);
                } else {
                    String str=getFilter(list, i, endIndex);
                    ret += str.substring(1, str.length()-1);
                    str=null;
                }
            }
        }
        ret += ")";
        list = null;
        return ret;
    }

    public String generateFilter() throws MessagingException {
        String ret = "";
        List list = getSortedAttrs();
        ret += getFilter(list, 0, list.size() - 1);
        return ret;
    }
}
