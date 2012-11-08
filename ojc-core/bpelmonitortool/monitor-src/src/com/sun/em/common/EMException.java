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
package com.sun.em.common;

/**
 * <code>EMException</code> class.
 *
 * @author <a href="mailto:omontoya@seebeyond.com"></a>
 * @version $Revision: 1.1 $
 */
public class EMException extends Exception {
    /**
     * Creates a new <code>EMException</code> instance.
     *
     */
    public EMException() {
        super();
    }

    /**
     * Creates a new <code>EMException</code> instance.
     *
     * @param e a <code>String</code> parameter.
     */
    public EMException(String e) {
        super(e);
    }
}
