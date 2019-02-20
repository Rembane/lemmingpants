module Components.Admin.ManageAgenda where

import Prelude

import Affjax as AX
import Affjax.RequestBody as Req
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as Res
import Affjax.StatusCode (StatusCode(..))
import Data.Array as A
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import FormHelpers (FieldError, fieldScaffolding, isNonEmpty)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Postgrest (pgrequest, useToken)
import Postgrest as PG
import Types.Agenda (Agenda, AgendaItem(..), _AgendaItems)
import Types.Flash as FL
import Types.Token (Token)

type State =
  { agenda :: Agenda
  , token  :: Token }

data Query a
  = Formless (F.Message' Form) a

type ChildQuery = F.Query' Form Aff

data Message
  = Flash (Maybe FL.Flash)

newtype Form r f = Form (r
  ( raw_agenda_items :: f FieldError String String
  ))
derive instance newtypeForm :: Newtype (Form r f) _

component :: H.Component HH.HTML Query State Message Aff
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery Unit Aff
    render state =
      HH.section_
        [ HH.slot
            unit
            F.component
              { initialInputs, validators, render: renderFormless }
              (HE.input Formless)
        , HH.table_ (
            [ HH.tr_
              [ HH.th_ [HH.text "Super title"]
              , HH.th_ [HH.text "Title"]
              ]
            ] <>
            (A.fromFoldable
              (map
                (\(AgendaItem {parent, title}) ->
                  HH.tr_
                    [ HH.td_ [HH.text (maybe "" (\(AgendaItem pai) -> pai.title) parent)]
                    , HH.td_ [HH.text title]
                    ]
                )
                (view _AgendaItems state.agenda)
              )
            )
          )
        ]
      where
        proxy = F.FormProxy :: F.FormProxy Form

        initialInputs :: Form Record F.InputField
        initialInputs = F.mkInputFields proxy

        validators :: Form Record (F.Validation Form Aff)
        validators = Form
          { raw_agenda_items: isNonEmpty }

        renderFormless :: F.State Form Aff -> F.HTML' Form Aff
        renderFormless s =
          HH.form
            [ HE.onSubmit (HE.input_ F.submit) ]
            [ HH.fieldset_
              [ HH.legend_ [HH.text "Import agenda items"]
              , HH.p_ [HH.text "The JSON should have the format:"]
              , HH.pre_ [HH.text "[{ \"title\": \"Title of the agenda item\",\n   \"children\": [{\"title\": \"Title of a child\", \"children\": [] }]\n}]"]
              , HH.p_ [HH.text "The agenda items will be created in the order they occur in the uploaded JSON blob."]
              , fieldScaffolding "Paste your JSON here"
                [ HH.textarea
                  [ HP.rows 5
                  , HP.value $ F.getInput r.raw_agenda_items s.form
                  , HE.onValueInput $ HE.input $ F.setValidate r.raw_agenda_items
                  , HP.required true
                  ]
                ]
              , HH.p_
                [ HH.input
                  [ HP.type_ HP.InputSubmit
                  , HP.value "Import agenda items!"
                  ]
                ]
              ]
            ]
          where
            r = F.mkSProxies proxy

    eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Message Aff
    eval =
      case _ of
        Formless m next -> do
          case m of
            F.Submitted formOutput -> do
              let {raw_agenda_items} = F.unwrapOutputFields formOutput
              {token} <- H.get
              {status} <- H.liftAff (
                let pg' = pgrequest (PG.createURL "/rpc/create_agenda")
                 in AX.request (pg' { headers        = pg'.headers <> [useToken token, RequestHeader "Prefer" "params=single-object"]
                                    , method         = Left POST
                                    , content        = Just $ Req.string $ raw_agenda_items
                                    , responseFormat = Res.string
                                    }))
              case status of
                StatusCode 200 ->
                  pure next
                _ ->
                  (H.raise $ Flash $ Just $
                    FL.mkFlash
                      "While trying to create and agenda -- ERROR! Got a HTTP response we didn't expect! See the console for more information."
                      FL.Error)
                  *> pure next
            _ -> pure next


