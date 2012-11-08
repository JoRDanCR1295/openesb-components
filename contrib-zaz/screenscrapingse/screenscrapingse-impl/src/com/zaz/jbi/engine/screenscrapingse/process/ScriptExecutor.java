/*
 * ScriptExecutor.java
 *
 * Created on February 23, 2007, 12:06 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import javax.xml.transform.Source;

/**
 *
 * @author Prashanth B.R
 */
public interface ScriptExecutor {

    /**
     * This is the core script processing method, which will accept a Dom source file
     * contain script data\meta data required for script execution. The result is returned as
     * another source content.
     *
     * @param aInputSource DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Source processScript(Source aInputSource);
} //interface ScriptExecutor ends.
