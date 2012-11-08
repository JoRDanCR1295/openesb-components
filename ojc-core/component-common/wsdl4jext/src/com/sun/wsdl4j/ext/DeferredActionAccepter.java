/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)DeferredActionAccepter.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

/**
 * This interface defines a behavior of accepting an opportunity to do
 * deferred resolution. An instance of the implementation class of this
 * interface basically has deferred resolution work to do (e.g., resolve
 * some member variables) and the instance does not know when is the best
 * time to do the work. But once the time comes, it will be provided an
 * opportunity to do the work.
 * 
 * @author Jun Xu
 */
public interface DeferredActionAccepter {

    /**
     * Performs defeerred action such as Resolving everything that needs
     * to be resolved.
     */
    void performDeferredAction();
}
