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
 * @(#)$Id: MainContext.java,v 1.3 2007/11/21 02:32:13 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;


public class MainContext extends CommandContext {

	public MainContext(String name, CommandContext parent, ContextResource resource) {
		super(name, parent, resource);
		// TODO Auto-generated constructor stub
		mChildCommands.put("a", new ShowDeployedBPELProcessesCommand ("ShowDeployedBPELProcesses", this));
		mChildCommands.put("b", new ShowBPELInstancesStatusCommand ("ShowBPELInstancesStatus", this));
		mChildCommands.put("c", new ShowActivityStatusOnBPELInstanceCommand ("ShowActivityStatusOnBPELInstance", this));
		mChildCommands.put("f", new GetInstanceFaultCommand ("GetInstanceFault", this));
		mChildCommands.put("s", new SuspendBPELInstanceCommand ("SuspendBPELInstance", this));
		mChildCommands.put("r", new ResumeBPELInstanceCommand ("ResumeBPELInstance", this));
		mChildCommands.put("t", new TerminateBPELInstanceCommand ("TerminateBPELInstance", this));	
		mChildCommands.put("l", new ViewBPELVariablesCommand ("ViewBPELVariables", this));
		mChildCommands.put("v", new ViewBPELVariableValueCommand ("ViewBPELVariableValue", this));
		mChildCommands.put("k", new ChangeBPELVariableValueCommand ("ChangeBPELVariableValue", this));
		mChildCommands.put("pr", new ShowParentBPELInstancesStatusCommand ("ShowParentBPELInstancesStatus", this));
		mChildCommands.put("ch", new ShowSubBPELInstancesStatusCommand ("ShowSubBPELInstancesStatus", this));
	}


}
