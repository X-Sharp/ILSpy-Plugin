USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows
USING System.Windows.Controls
USING System.Windows.Data
USING System.Windows.Documents
USING System.Windows.Input
USING System.Windows.Media
USING System.Windows.Media.Imaging
USING System.Windows.Navigation
USING System.Windows.Shapes
USING ICSharpCode.ILSpy
USING ICSharpCode.ILSpy.Options
USING ICSharpCode.ILSpy.Util
USING System.Composition
USING System.ComponentModel
USING System.Xml.Linq

BEGIN NAMESPACE ILSpy.XSharpLanguage

    /// <summary>
    /// Option page for XSharp decompiler settings.
    /// </summary>
    [ExportOptionPage];
    PUBLIC PARTIAL CLASS XSharpOptionPage INHERIT UserControl IMPLEMENTS IOptionPage

        STATIC INITONLY ns := "XSharp" AS STRING
        PRIVATE STATIC _currentSettings AS XSharpOptions

        PUBLIC PROPERTY Title AS STRING
            GET
                RETURN "XSharp"
            END GET
        END PROPERTY

        #region Access Settings

        PUBLIC STATIC PROPERTY CurrentXSharpSettings AS XSharpOptions
            GET
                IF _currentSettings == NULL
                    _currentSettings := XSharpOptions{}
                ENDIF
                RETURN _currentSettings
            END GET
        END PROPERTY

            #endregion

        PUBLIC CONSTRUCTOR()
            LOCAL cbUpperKeyword AS CheckBox
            LOCAL cbIfStatement AS CheckBox
            LOCAL sp AS StackPanel
            LOCAL grid AS Grid
            //
            cbUpperKeyword := CheckBox{}
            cbUpperKeyword:Content := "Output Keywords in Uppercase"
            cbUpperKeyword:SetBinding(CheckBox.IsCheckedProperty, Binding{"UpperKeyword"})
            //
            cbIfStatement := CheckBox{}
            cbIfStatement:Content := "Surround IF statement with parenthesis"
            cbIfStatement:SetBinding(CheckBox.IsCheckedProperty, Binding{"IfStatement"})
            //
            sp := StackPanel{}
            sp:Children:Add(cbUpperKeyword)
            sp:Children:Add(cbIfStatement)
            //
            grid := Grid{}
            grid:Margin := System.Windows.Thickness{10, 10, 10, 10}
            grid:Children:Add(sp)
            //
            SELF:Content := grid
            RETURN


        PUBLIC METHOD Load( settings AS SettingsSnapshot ) AS VOID
            LOCAL s := XSharpOptions{} AS XSharpOptions
            //
            SELF:DataContext := s
            _currentSettings := s

		PUBLIC METHOD LoadDefaults( ) AS VOID
			// Create without params, so ... default value
            LOCAL s := XSharpOptions{ } AS XSharpOptions
            //
            SELF:DataContext := s

        PUBLIC METHOD Save( root AS XElement ) AS VOID
            LOCAL s := (XSharpOptions)SELF:DataContext AS XSharpOptions
            // Save the options back into XML:
            LOCAL section := XElement{ns + "Options"} AS XElement
            section:SetAttributeValue("upperkeyword", s:UpperKeyword)
			section:SetAttributeValue("ifstatement", s:IfStatement)
            // Replace the existing section in the settings file, or add a new section,
            // if required.
            LOCAL existingElement := root:Element(ns + "Options") AS XElement
            IF (existingElement != NULL)
                existingElement:ReplaceWith(section)
            ELSE
                root:Add(section)
            ENDIF

            END CLASS


    PUBLIC CLASS XSharpOptions INHERIT INotifyPropertyChanged

        PRIVATE _upperkeyword AS LOGIC
		PRIVATE _ifstatement AS LOGIC

        PUBLIC CONSTRUCTOR()
            SELF:_upperkeyword := TRUE
			SELF:_ifstatement := TRUE

        PUBLIC CONSTRUCTOR( e AS XElement )
            LOCAL attr AS XAttribute
            //
            attr := e:Attribute("upperkeyword")
            IF ( attr != NULL )
                LOCAL val := attr:Value AS STRING
                LOCAL bVal := TRUE AS LOGIC
                IF Boolean.TryParse( val, OUT bVal )
                    SELF:_upperkeyword := bVal
                ELSE
                    SELF:_upperkeyword := TRUE
                ENDIF
            ENDIF
            //
            attr := e:Attribute("ifstatement")
            IF ( attr != NULL )
                LOCAL val := attr:Value AS STRING
                LOCAL bVal := TRUE AS LOGIC
                IF Boolean.TryParse( val, OUT bVal )
                    SELF:_ifstatement := bVal
                ELSE
                    SELF:_ifstatement := TRUE
                ENDIF
            ENDIF
            //


        PUBLIC PROPERTY UpperKeyword AS LOGIC
            GET
            RETURN _upperkeyword
            END GET

            SET
                IF (_upperkeyword != VALUE)
                    _upperkeyword := VALUE
                    OnPropertyChanged("UpperKeyword")
                ENDIF
            END SET
		END PROPERTY

        PUBLIC PROPERTY IfStatement AS LOGIC
            GET
            RETURN _ifstatement
            END GET

            SET
                IF (_ifstatement != VALUE)
                    _ifstatement := VALUE
                    OnPropertyChanged("IfStatement")
                ENDIF
            END SET
        END PROPERTY

        PUBLIC EVENT PropertyChanged AS PropertyChangedEventHandler

    PROTECTED VIRTUAL METHOD OnPropertyChanged( propertyName AS STRING ) AS VOID
        IF (PropertyChanged != NULL)
            PropertyChanged(SELF, PropertyChangedEventArgs{propertyName})
        ENDIF

    END CLASS

END NAMESPACE
