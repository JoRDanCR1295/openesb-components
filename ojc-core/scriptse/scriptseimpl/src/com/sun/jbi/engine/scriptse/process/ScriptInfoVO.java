/*
 * ScriptInfoVO.java
 *
 * Created on 9 April, 2007, 4:33 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.engine.scriptse.process;

import com.sun.jbi.engine.scriptse.scrconfig.Scriptconfig;



/**
 * DOCUMENT ME!
 *
 * @author prashanthbr
 */
public class ScriptInfoVO {
    private Scriptconfig mScriptCfg = null;
    private String mScriptFileName = null;
    private String mEngineName = null;
    private WorkingMode mWorkMode = WorkingMode.CMPLX_MODE;

    /**
     * Creates a new instance of ScriptInfoVO
     */
    public ScriptInfoVO() {
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Scriptconfig getMScriptCfg() {
        return mScriptCfg;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mScriptCfg DOCUMENT ME!
     */
    public void setMScriptCfg(Scriptconfig mScriptCfg) {
        this.mScriptCfg = mScriptCfg;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public WorkingMode getMWorkMode() {
        return mWorkMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public String getMEngineName() {
        return mEngineName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mEngineName
     */
    public void setMEngineName(String mEngineName) {
        this.mEngineName = mEngineName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mWorkMode
     */
    public void setMWorkMode(WorkingMode mWorkMode) {
        this.mWorkMode = mWorkMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return
     */
    public String getMScriptFileName() {
        return mScriptFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mScriptFileName
     */
    public void setMScriptFileName(String mScriptFileName) {
        this.mScriptFileName = mScriptFileName;
        //Setting the Script file, indicates that SE is working in a simple Mode
        // wherein the script is part of SU and only input parameters are passed to it.
        this.setMWorkMode(WorkingMode.SIMPLE_MODE);
    }

/**
   *
   */
    public enum WorkingMode {CMPLX_MODE,
        SIMPLE_MODE;
    }
} //class ScriptInfoVO ends.
