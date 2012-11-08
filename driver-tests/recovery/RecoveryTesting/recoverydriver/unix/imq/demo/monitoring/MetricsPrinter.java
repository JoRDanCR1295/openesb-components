/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)MetricsPrinter.java	1.3 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

/*
 * Class used to print metrics information received from the MQ broker.
 */
public class MetricsPrinter extends MultiColumnPrinter  {
    public MetricsPrinter(int numCols, int gap, String border, int align)  {
	super(numCols, gap, border, align);
    }

    public MetricsPrinter(int numCols, int gap, String border)  {
	super(numCols, gap, border);
    }

    public void doPrint(String str)  {
        System.out.print(str);
    }

    public void doPrintln(String str)  {
        System.out.println(str);
    }
}
