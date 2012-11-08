/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.scriptse;

/**
 * ScriptSEConfigurationMBean.java
 *
 * Created on May 23, 2005, 1:44 PM
 *
 * @author Bing Lu
 */
public interface ScriptseConfigurationMBean {
    /**
     * DOCUMENT ME!
     *
     * @param someValue DOCUMENT ME!
     */
    public void setSomeProperty(String someValue);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getSomeProperty();
}
