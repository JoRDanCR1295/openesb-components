/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

/**
 *
 * @author tianlize
 */
public class QueryAttributeBean {

    private int positionIndex;
    private int bracketDepth;
    private int bracketBeginDepth;
    private int bracketEndDepth;
    private String logicOp;
    private String compareOp;

    public QueryAttributeBean() {
        
    }

    public QueryAttributeBean(int positionIndex, int bracketDepth, int bracketBeginDepth,
            int bracketEndDepth, String logicOp, String compareOp) {
        this.positionIndex = positionIndex;
        this.bracketDepth = bracketDepth;
        this.bracketBeginDepth = bracketBeginDepth;
        this.bracketEndDepth = bracketEndDepth;
        this.logicOp = logicOp;
        this.compareOp = compareOp;
    }
    
    public String toString(){
        String ret="";
        ret+=positionIndex;
        ret+=" "+bracketDepth;
        ret+=" "+bracketBeginDepth;
        ret+=" "+bracketEndDepth;
        ret+=" "+logicOp;
        ret+=" "+compareOp+"\n";
        return ret;        
    }

    public int getBracketBeginDepth() {
        return bracketBeginDepth;
    }

    public void setBracketBeginDepth(int bracketBeginDepth) {
        this.bracketBeginDepth = bracketBeginDepth;
    }

    public int getBracketDepth() {
        return bracketDepth;
    }

    public void setBracketDepth(int bracketDepth) {
        this.bracketDepth = bracketDepth;
    }

    public int getBracketEndDepth() {
        return bracketEndDepth;
    }

    public void setBracketEndDepth(int bracketEndDepth) {
        this.bracketEndDepth = bracketEndDepth;
    }

    public String getCompareOp() {
        return compareOp;
    }

    public void setCompareOp(String compareOp) {
        this.compareOp = compareOp;
    }

    public String getLogicOp() {
        return logicOp;
    }

    public void setLogicOp(String logicOp) {
        this.logicOp = logicOp;
    }

    public int getPositionIndex() {
        return positionIndex;
    }

    public void setPositionIndex(int positionIndex) {
        this.positionIndex = positionIndex;
    }
}
