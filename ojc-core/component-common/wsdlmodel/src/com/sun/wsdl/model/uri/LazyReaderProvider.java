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
 * @(#)LazyReaderProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri;

import java.io.IOException;
import java.io.Reader;

/**
 * Describes the interface for a "lazy" reader provider -- one that computes the
 * reader only on demand (done to avoid opened file handles when a reader is not
 * consumed).
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface LazyReaderProvider {
    
    /** Computes the reader on demand.
     * @return  Freshly opened <code>Reader</code> object to a certain resource.
     * @throws  Exception   When I/O problems occur.
     */
    Reader computeReader() throws IOException;
    
}
