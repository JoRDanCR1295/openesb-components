/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Command.java - ver 1.EA7 - 12/10/2005
 *
 * Copyright 2004-2005 Sun Microsystems, Inc. All Rights Reserved.
 */

package com.sun.jbi.thread;

/**
 * This interface encapsulates a service request. Clients create concrete
 * implementations of this interface and notify the WorkManager to execute the
 * command in a free thread.
 *
 * @author Praveen Patange
  */
public interface Command
{
    /**
     * This method will contain the code to be executed. WorkManager will
     * invoke this method in a free thread.
     */
    void execute();
}
