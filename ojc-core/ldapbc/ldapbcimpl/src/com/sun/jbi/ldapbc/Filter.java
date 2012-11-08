/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

/**
 *
 * @author tianlize
 */
public class Filter {

    private int positionIndex;
    private String objName;
    private String logicOp;
    private String attributeName;
    private String compareOp;
    private String value;
    private int bracketDepth;
    private int bracketBeginDepth;
    private int bracketEndDepth;
    
    public Filter(){
        
    }

    public Filter(int posIndex, String objName, String logicOp, String attrName,
            String compareOp, int bDepth, int bBeginDepth, int bEndDepth, String value) {
        this.positionIndex = posIndex;
        this.objName = objName;
        this.logicOp = logicOp;
        this.attributeName = attrName;
        this.compareOp = compareOp;
        this.bracketBeginDepth = bBeginDepth;
        this.bracketDepth = bDepth;
        this.bracketEndDepth = bEndDepth;
        this.value = value;
    }

    public void reduceBracketEndDepth() {
        if (bracketEndDepth > 0) {
            bracketEndDepth--;
        }
    }

    public void reduceBracketBeginDepth() {
        if (bracketBeginDepth > 0) {
            bracketBeginDepth--;
        }
    }

    public void reduceBracketDepth() {
        if (bracketDepth > 0) {
            bracketDepth--;
        }
    }

    public boolean isBeginBracket() {
        return bracketBeginDepth > 0;
    }

    public boolean isEndBracket() {
        return bracketEndDepth > 0;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
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

    public String getObjName() {
        return objName;
    }

    public void setObjName(String objName) {
        this.objName = objName;
    }

    public int getPositionIndex() {
        return positionIndex;
    }

    public void setPositionIndex(int positionIndex) {
        this.positionIndex = positionIndex;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
    }
