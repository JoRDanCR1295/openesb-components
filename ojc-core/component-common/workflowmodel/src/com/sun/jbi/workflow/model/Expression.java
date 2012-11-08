/*
 * Expression.java
 * 
 * Created on Jun 27, 2007, 2:23:00 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import org.apache.commons.jxpath.JXPathContext;

/**
 *
 * @author radval
 */
public interface Expression {

    String getContent(JXPathContext context);

}
