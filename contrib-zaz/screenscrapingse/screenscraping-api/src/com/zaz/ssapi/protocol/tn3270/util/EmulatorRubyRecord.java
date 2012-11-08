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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.tn3270.util;

import com.zaz.ssapi.protocol.tn3270.ProtocolHandler3270;
import com.zaz.ssapi.protocol.common.model.CustomizeOutputField;

import com.zaz.ssapi.protocol.common.model.RowColumnDef;
import com.zaz.ssapi.protocol.tn3270.model.jaxb.Emulatorflow;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author liyunhai
 */
public class EmulatorRubyRecord {

    private static FileWriter fw = null;
    private static PrintWriter out = null;
    private static boolean recordFlag = false;
    private static List<String> paramList = new ArrayList<String>();

    private EmulatorRubyRecord() {
    }

    public static boolean isRecordFlag() {
        return recordFlag;
    }

    public static void setRecordFlag(boolean recordFlag) {
        EmulatorRubyRecord.recordFlag = recordFlag;
    }

    public static List<String> getParamList() {
        return paramList;
    }

    public static void generateRubyFile(File dir, String fileName, ProtocolHandler3270 handler) {
        recordBegin(dir, fileName, handler.getEmulatorflow().getHost(), handler.getEmulatorflow().getPort());
        for (int i = 0; i < handler.getEmulatorflow().getScreen().size(); i++) {
            Emulatorflow.Screen screen = handler.getEmulatorflow().getScreen().get(i);
            if (i < handler.getEmulatorflow().getScreen().size() - 1) {
                recordAction(screen.getAction());
                recordCursorAddr(screen.getCursoraddress());
            }
            if (screen.getOutput() == 1) {
                recordBackup();
            } else if (screen.getOutput() == 2) {
                recordScrollDown();
                recordScrollUp();
            }
            if (i < handler.getEmulatorflow().getScreen().size() - 1) {
                for (int j = 0; j < screen.getField().size(); j++) {
                    Emulatorflow.Screen.Field field = screen.getField().get(j);
                    recordField(field);
                }
                recordWrite();
                recordRead();
            }
        }
        recordEnd(handler);
    }

    public static void recordBegin(File dir, String fileName, String host,
            int port) {
        try {
            fw = new FileWriter(dir.getAbsolutePath() + File.separator + "src" +
                    File.separator + fileName);
            out = new PrintWriter(fw);
        } catch (IOException ex) {
            Logger.getLogger(EmulatorRubyRecord.class.getName()).log(Level.SEVERE, null, ex);
        }

        recordFlag = true;

        out.println("require \"java\"");
        out.println();
        out.println("import \"com.zaz.ssapi.protocol.tn3270.TN3270\"");
        out.println();
        out.println("def function_xxx()");
        out.println();
        out.println("  tn3270 = TN3270.new");
        out.println();
        out.println("  $response.setStatus(\"fail\")");
        out.println();
        recordConnect(host, port);
    }

    private static void recordInvoke() {
        out.println("def invoke_function_xxx(request)");
        out.println();

        for (int i = 0; i < paramList.size(); i++) {
            String paramName = paramList.get(i);
            String firstChar = paramName.substring(0, 1).toUpperCase();
            String upperName = firstChar + paramName.substring(1);
            String lineText = String.format("  @%s = request.get%s()",
                    paramName, upperName);
            out.println(lineText);
            out.println();
        }

        out.println("  function_xxx()");
        out.println();
        out.println("end");
        out.println();

        out.println("invoke_function_xxx($request)");
    }

//    private static void recordAssignResponse(ProtocolHandler3270 handler) {
//        List fields = handler.getOutputInfoTotal().getField();
//
//        for (int i = 0; i < fields.size(); i++) {
//            OutputField oField = (OutputField) fields.get(i);
//            int screenIndex = oField.getScreenIndex();
//            int startPos = oField.getStartPos();
//            int length = oField.getLength();
//            String name = oField.getName();
//            String firstChar = name.substring(0, 1).toUpperCase();
//            String upperName = firstChar + name.substring(1);
//            String lineText = String.format("  $response.set%s(tn3270.GetOutputFieldValue(%d, %d, %d))",
//                    upperName, screenIndex, startPos, length);
//            out.println(lineText);
//            out.println();
//        }
//
//        out.println("  return $response");
//        out.println();
//    }
    private static void recordCustomizeResponse(ProtocolHandler3270 handler) {
        String lineText = "";
        List<String> getScreenMethodDef = new ArrayList<String>();
        List<String> getValueMethodDef = new ArrayList<String>();
        List<String> getRowColMethodDef = new ArrayList<String>();
        Map<Integer, List<CustomizeOutputField>> fields = handler.getCusOutputInfo().getOutputInfo();

        for (int i = 0; i < fields.size(); i++) {
            lineText = String.format("  get_screen%d(tn3270)", i);
            out.println(lineText);
            out.println("  ");
            getScreenMethodDef.add(String.format("def get_screen%d(tn3270)\n", i));
            getScreenMethodDef.add(String.format("  screens_buffer = tn3270.GetPresentationSpace(%d)\n", i));
            for (int j = 0; j < fields.get(i).size(); j++) {
                CustomizeOutputField oField = (CustomizeOutputField) fields.get(i).get(j);
                String name = oField.getXsdName();
                String firstChar = name.substring(0, 1).toUpperCase();
                String xsdName = firstChar + name.substring(1);
                switch (oField.getFieldType()) {
                    case CustomizeOutputField.SINGLE_VALUE:
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        lineText += String.format("  single_value =  tn3270.GetSingleValueByKeyword(screens_buffer, \"%s\", \"%s\", %d)\n",
                                oField.getKeyword(), oField.getSeparator(), oField.getLength());
                        lineText += String.format("  $response.getScreen%d().set%s(single_value)\n", i, xsdName);
                        lineText += String.format("end\n");
                        getValueMethodDef.add(lineText);
                        getScreenMethodDef.add(String.format("  get_%s(tn3270, screens_buffer)\n", name));
                        break;
                    case CustomizeOutputField.MULTI_VALUE:
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        lineText += String.format("  multi_value = tn3270.GetMultiValueByKeyword(screens_buffer, \"%s\", \"%s\", \"%s\", %d)\n",
                                oField.getKeyword(), oField.getSeparator(), oField.getSubSeparator(), oField.getLength());
                        //lineText += String.format("  $response.getScreen%d().set%s(multi_value)\n", i, xsdName);
                        lineText += "  if multi_value != nil\n";
                        lineText += "    multi_value.each {|value|\n";
                        lineText += String.format("      $response.getScreen%d().get%s().add(value)}\n", i, xsdName);
                        lineText += String.format("  else\n");
                        lineText += String.format("      $response.getScreen%d().get%s().add(\"\")\n", i, xsdName);
                        lineText += String.format("  end\n");
                        lineText += String.format("end\n");
                        getValueMethodDef.add(lineText);
                        lineText = String.format("  get_%s(tn3270, screens_buffer)\n", name);
                        getScreenMethodDef.add(lineText);
                        break;
                    case CustomizeOutputField.RELATION_TABLE_VALUE:
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        for (int col = 0; col < oField.getRowCols().size(); col++) {
                            RowColumnDef columns = (RowColumnDef) oField.getRowCols().get(col);
                            String colName = columns.getXsdName();
                            String colFirstChar = colName.substring(0, 1).toUpperCase();
                            String colXsdName = colFirstChar + colName.substring(1);
                            lineText += String.format("  get_%s(tn3270, screens_buffer)\n", colName);
                            getRowColMethodDef.add(String.format("def get_%s(tn3270, screens_buffer)\n", colName));
                            getRowColMethodDef.add(String.format("  column_value = tn3270.GetRelationTableValue(screens_buffer, \"%s\", %d, %d, %d)\n",
                                    oField.getTableHead(), oField.getOffset(), columns.getColumnBegin(), columns.getColumnEnd()));
                            getRowColMethodDef.add(String.format("  $response.getScreen%d().get%s().set%s(column_value)\n", i, xsdName, colXsdName));
                            getRowColMethodDef.add("end\n");
                        }
                        lineText += String.format("end\n");
                        getValueMethodDef.add(lineText);
                        getScreenMethodDef.add(String.format("  get_%s(tn3270, screens_buffer)\n", name));
                        break;
                    case CustomizeOutputField.CHILD_NODE_VALUE:
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        lineText += String.format("  child_value = tn3270.GetChildNodeValue(screens_buffer, \"%s\")\n", oField.getTreeNode());
                        lineText += String.format("  $response.getScreen%d().set%s(child_value)\n", i, xsdName);
                        lineText += String.format("end\n");
                        getValueMethodDef.add(lineText);
                        getScreenMethodDef.add(String.format("  get_%s(tn3270, screens_buffer)\n", name));
                        break;
                    case CustomizeOutputField.MAP_TABLE_VALUE:
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        for (int row = 0; row < oField.getRowCols().size(); row++) {
                            RowColumnDef rows = (RowColumnDef) oField.getRowCols().get(row);
                            String rowName = rows.getXsdName();
                            String rowFirstChar = rowName.substring(0, 1).toUpperCase();
                            String rowXsdName = rowFirstChar + rowName.substring(1);
                            lineText += String.format("  get_%s(tn3270, screens_buffer)\n", rowName);
                            getRowColMethodDef.add(String.format("def get_%s(tn3270, screens_buffer)\n", rowName));
                            getRowColMethodDef.add(String.format("  row_value = tn3270.GetMapTableValue(screens_buffer, \"%s\", %d, \"%s\", \"%s\", %d)\n",
                                    oField.getTableHead(), oField.getOffset(), rows.getName(), rows.getSeparator(), rows.getValueLength()));
                            getRowColMethodDef.add(String.format("  $response.getScreen%d().get%s().set%s(row_value)\n", i, xsdName, rowXsdName));
                            getRowColMethodDef.add("end\n");
                        }
                        lineText += String.format("end\n");
                        getValueMethodDef.add(lineText);
                        getScreenMethodDef.add(String.format("  get_%s(tn3270, screens_buffer)\n", name));
                        break;
                    case CustomizeOutputField.LIST_TABLE_VALUE:
                        String listColumns = "";
                        lineText = String.format("def get_%s(tn3270, screens_buffer)\n", name);
                        for (int row = 0; row < oField.getRowCols().size(); row++) {
                            RowColumnDef rows = (RowColumnDef) oField.getRowCols().get(row);
                            if (listColumns.equals("")) {
                                listColumns = String.format("%d,%d,%d", rows.getRow(), rows.getColumnBegin(), rows.getColumnEnd());
                            } else {
                                listColumns += String.format(",%d,%d,%d", rows.getRow(), rows.getColumnBegin(), rows.getColumnEnd());
                            }
                        }
                        lineText += String.format("  list_value = tn3270.GetListTableValue(screens_buffer, %d, %d, %d, %d, \"%s\")\n",
                                oField.getOffset(), oField.getLength(), oField.getValueBeginRow(), oField.getValueEndRow(), listColumns);
                        lineText += "  get_fields(list_value)\n";
                        lineText += String.format("end\n");
                        getRowColMethodDef.add("def get_fields(list_value)\n");
                        getRowColMethodDef.add("  i = 0\n");
                        getRowColMethodDef.add(String.format("  field = $response.getScreen%d().get%s().getFields().get(0)\n", i, xsdName));
                        getRowColMethodDef.add(String.format("  $response.getScreen%d().get%s().getFields().clear()\n", i, xsdName));
                        getRowColMethodDef.add("  while i < list_value.length\n");
                        getRowColMethodDef.add("    temp_field = field.getClass().newInstance()\n");
                        for (int row = 0; row < oField.getRowCols().size(); row++) {
                            RowColumnDef rows = (RowColumnDef) oField.getRowCols().get(row);
                            String rowName = rows.getXsdName();
                            String rowFirstChar = rowName.substring(0, 1).toUpperCase();
                            String rowXsdName = rowFirstChar + rowName.substring(1);
                            getRowColMethodDef.add(String.format("    temp_field.set%s(list_value[i])\n", rowXsdName, row));
                            getRowColMethodDef.add("    i = i + 1\n");
                        }
                        getRowColMethodDef.add(String.format("    $response.getScreen%d().get%s().getFields().add(temp_field)\n", i, xsdName));
                        getRowColMethodDef.add("  end\n");
                        getRowColMethodDef.add("end\n");
                        getValueMethodDef.add(lineText);
                        getScreenMethodDef.add(String.format("  get_%s(tn3270, screens_buffer)\n", name));
                        break;
                    default:
                        break;
                }
            }
            getScreenMethodDef.add(String.format("end\n"));
        }
        out.println("  $response.setStatus(\"success\")");
        out.println();
        out.println("  return $response");
        out.println("end");
        out.println("  ");
        for (int i = 0; i < getScreenMethodDef.size(); i++) {
            out.println((String) getScreenMethodDef.get(i));
        }

        for (int i = 0; i < getValueMethodDef.size(); i++) {
            out.println((String) getValueMethodDef.get(i));
        }

        for (int i = 0; i < getRowColMethodDef.size(); i++) {
            out.println((String) getRowColMethodDef.get(i));
        }
    }

    public static void recordEnd(ProtocolHandler3270 handler) {
        recordDisconnect();

        recordCustomizeResponse(handler);

        recordInvoke();

        out.flush();
        out.close();
        out = null;

        try {
            fw.close();
            fw = null;
        } catch (IOException ex) {
            Logger.getLogger(EmulatorRubyRecord.class.getName()).log(Level.SEVERE, null, ex);
        }

        recordFlag = false;
    }

    private static void recordConnect(String host, int port) {
        out.println("  tn3270.ConnectPresentationSpace(\"" + host + "\", \"" +
                String.valueOf(port) + "\")");
        out.println();

        recordRead();
    }

    private static void recordDisconnect() {
        out.println("  tn3270.DisconnectPresentationSpace()");
        out.println();
    }

    public static void recordRead() {
        if (!recordFlag) {
            return;
        }

        out.println("  tn3270.CopyPresentationSpace()");
        out.println();
    }

    public static void recordBackup() {
        if (!recordFlag) {
            return;
        }

        out.println("  tn3270.BackupPresentationSpace()");
        out.println();
    }

    public static void recordScrollDown() {
        if (!recordFlag) {
            return;
        }

        out.println("  tn3270.ScrollDownPresentationSpace()");
        out.println();
    }

    public static void recordScrollUp() {
        if (!recordFlag) {
            return;
        }

        out.println("  tn3270.ScrollUpPresentationSpace()");
        out.println();
    }

    public static void recordWrite() {
        if (!recordFlag) {
            return;
        }

        out.println("  tn3270.SendPresentationSpace()");
        out.println();
    }

    public static void recordField(Emulatorflow.Screen.Field field) {
        if (!recordFlag) {
            return;
        }
        String lineText = "";
        if (field.isIsparam()) {
            lineText += "  # Input: \"";
            lineText += field.getValue();
            lineText += "\", ";
            lineText += "Comment: ";
            lineText += field.getComment();
            out.println(lineText);

            lineText = String.format("  tn3270.CopyStringToField(%d, @%s)",
                    field.getIndex(), field.getParaname());

            paramList.add(field.getParaname());
        } else {
            String newValue = field.getNewvalue();
            if (!"".equals(newValue)) {
                lineText = String.format("  tn3270.CopyStringToField(%d, \"%s\")",
                        field.getIndex(), newValue);
            } else {
                lineText = String.format("  tn3270.CopyStringToField(%d, \"%s\")",
                        field.getIndex(), field.getValue());
            }


        }

        out.println(lineText);
        out.println();
    }

    public static void recordAction(int action) {
        if (!recordFlag) {
            return;
        }

        String lineText = String.format("  tn3270.SetAction(%d)", action);
        out.println(lineText);
        out.println();
    }

    public static void recordCursorAddr(int address) {
        if (!recordFlag) {
            return;
        }

        String lineText = String.format("  tn3270.SetCursor(%d)", address);
        out.println(lineText);
        out.println();
    }
}
