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
 * @(#)StateFactory.java 
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
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extservice.server;

import java.util.Set;

/**
 * StateFactory centralizes the creation of states
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class StateFactory {

    private Set mEmailListeners;

    public StateFactory(final Set emailListeners) {
        mEmailListeners = emailListeners;
    }

    public WelcomeState createWelcomeState() {
        return new WelcomeState(this);
    }

    public HeloState createHeloState(final State parent) {
        return new HeloState(this, parent);
    }

    public EhloState createEhloState(final State parent) {
        return new EhloState(this, parent);
    }

    public NoopState createNoopState(final State parent) {
        return new NoopState(this, parent);
    }

    public QuitState createQuitState(final State parent) {
        return new QuitState(this, parent);
    }

    public RsetState createRsetState() {
        return new RsetState(this);
    }

    public VrfyState createVrfyState(final State parent) {
        return new VrfyState(this, parent, mEmailListeners);
    }

    public MailState createMailState(final State parent) {
        return new MailState(this, parent);
    }

    public RcptState createRcptState(final State parent) {
        return new RcptState(this, parent, mEmailListeners);
    }

    public DataState createDataState(final State parent) {
        return new DataState(this, parent);
    }

    public DataReadState createDataReadState(final State parent) {
        return new DataReadState(this, parent);
    }

    public DataEndState createDataEndState(final State parent) {
        return new DataEndState(this, parent, mEmailListeners);
    }

}
