/* Useful macros */

#define ASSERTED(x) (x##_assertions > 0)
#define ASSERT(x) x##_assertions = x##_assertions + 1
#define RETRACT(x) x##_assertions = x##_assertions - 1

/* Global stuff */

/* Book Quote */
int BQ_assertions = 0;
int IBQ_assertions = 0;
int IIBQ_assertions = 0;

/* Book Interest */
int BI_assertions = 0;
int IBI_assertions = 0;
int IIBI_assertions = 0;

/* Club Members */
int CM_assertions = 0;
int ICM_assertions = 0;

/* Announcements */
int BoTM_assertions = 0;

/* Seller stuff */
mtype = { seller, seller_during};

active proctype Seller() {
  mtype current_state = seller;
  bool asserting_IIBQ = true;
  bool asserting_BQ = false;
  bool know_IBQ = false;
  ASSERT(IIBQ);
  do
    :: current_state == seller ->
       if
         :: ASSERTED(IBQ) && !know_IBQ ->
            current_state = seller_during;
            asserting_BQ = true;
            ASSERT(BQ);
       fi;
       know_IBQ = ASSERTED(IBQ);
    :: current_state == seller_during ->
       if
         :: !ASSERTED(IBQ) && know_IBQ ->
            current_state = seller;
            asserting_BQ = false;
            RETRACT(BQ);
       fi;
       know_IBQ = ASSERTED(IBQ);
  od;
}

mtype = { get_quotes, announce, poll, none };
mtype leader_state = get_quotes;

active proctype Leader() {
  bool asserting_IBI = false;
  bool asserting_BoTM = false;
  bool asserting_IBQ = true;
  bool asserting_ICM = true;
  bool know_BQ = false;
  bool know_BI = false;
  ASSERT(IBQ);
  ASSERT(ICM);
  do
    :: leader_state == get_quotes ->
       if
         :: ASSERTED(BQ) && !know_BQ ->
            leader_state = poll;
            asserting_IBI = true;
            ASSERT(IBI);
         :: ASSERTED(BQ) && !know_BQ ->
            leader_state = none;
            asserting_IBQ = false;
            asserting_ICM = false;
            RETRACT(IBQ);
            RETRACT(ICM);
       fi;
       know_BQ = ASSERTED(BQ)
    :: leader_state == announce ->
       skip;
    :: leader_state == poll ->
       if
         :: ASSERTED(BI) && !know_BI ->
            leader_state = get_quotes;
            assert(asserting_IBI);
            asserting_IBI = false;
            RETRACT(IBI);
            IBI_assertions = IBI_assertions - 1;
         :: ASSERTED(BI) && !know_BI ->
            leader_state = announce;
            assert(asserting_IBI);
            asserting_IBI = false;
            RETRACT(IBI);
            asserting_BoTM = true;
            ASSERT(BoTM);
         :: ASSERTED(BI) && !know_BI ->
            leader_state = none;
            assert(asserting_IBI);
            asserting_IBQ = false;
            asserting_ICM = false;
            asserting_IBI = false;
            RETRACT(IBQ);
            RETRACT(ICM);
            RETRACT(IBI);
         :: ASSERTED(BQ) && !know_BQ ->
            leader_state = none;
            assert(asserting_IBI);
            asserting_IBQ = false;
            asserting_ICM = false;
            asserting_IBI = false;
            RETRACT(IBQ);
            RETRACT(ICM);
            RETRACT(IBI);
       fi;
       know_BI = ASSERTED(BI);
       know_BQ = ASSERTED(BQ);
    :: leader_state == none ->
       skip;
  od
}

mtype = { member, during_member };

active proctype Member() {
  mtype current_state = member;
  bool asserting_BI = false;
  bool asserting_IIBI = true;
  bool asserting_CM = true;
  ASSERT(IIBI);
  ASSERT(CM);
  bool know_IBI = false;
  do
    :: current_state == member ->
       if
         :: ASSERTED(IBI) && !know_IBI ->
            current_state = during_member;
            asserting_BI = true;
            ASSERT(BI);
       fi;
       know_IBI = ASSERTED(IBI);
    :: current_state == during_member ->
       if
         :: !ASSERTED(IBI) && know_IBI ->
            current_state = member;
            asserting_BI = false;
            RETRACT(BI);
       fi;
       know_IBI = ASSERTED(IBI);
  od
}

ltl sanity {
  [](BQ_assertions >= 0 &&
     IBQ_assertions >= 0 &&
     IIBQ_assertions >= 0 &&
     BI_assertions >= 0 &&
     IBI_assertions >= 0 &&
     IIBI_assertions >= 0 &&
     CM_assertions >= 0 &&
     ICM_assertions >= 0 &&
     BoTM_assertions >= 0)
  &&
  <> (BQ_assertions > 0)
  /*
  &&
  <> (leader_state == announce || leader_state == none)
   */
}