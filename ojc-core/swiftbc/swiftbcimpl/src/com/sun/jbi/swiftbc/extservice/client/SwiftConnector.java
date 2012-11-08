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
 * @(#)HL7Connector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun MicroSystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun MicroSystems.
 *
 ***************************************************************************/

package com.sun.jbi.swiftbc.extservice.client;

import com.sun.jbi.swiftbc.extservice.ProtocolInfo;

/***************************************************************************************************
 * An Interface that all the transport protocol specific implementation classes must implement to
 * provide communication with the Swift External System
 * 
 * @author S. Nageswara Rao
 * @version 
 */
public interface SwiftConnector {

    // Establishes a connection with the Swift Extenral System/Server
    void connect(ProtocolInfo protocolInfo) throws Exception;

    // Sends SwiftMessage to Swift External System
    void sendSwiftMessage(String SwiftMsg) throws Exception;

    // Receives Swift Message from Swift External System
    String recvSwiftMessage() throws Exception;

    // Synchronous API for dealing with Swift External System
    String sendReceiveSwiftMessage(String SwiftMsg) throws Exception;

    //Disconnect the connection with the Swift External System/Server
    void disconnect() throws Exception;

}// end of the interface
